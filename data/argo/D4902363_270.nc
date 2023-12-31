CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       JMA    source        
Argo float     history       b2018-08-15T00:35:12Z creation;2018-08-15T00:35:18Z conversion to V3.1;2019-12-19T07:35:06Z update;     
references        (http://www.argodatamgt.org/Documentation   comment       	free text      user_manual_version       3.1    Conventions       Argo-3.1 CF-1.6    featureType       trajectoryProfile         @   	DATA_TYPE                  	long_name         	Data type      conventions       Argo reference table 1     
_FillValue                    6�   FORMAT_VERSION                 	long_name         File format version    
_FillValue                    6�   HANDBOOK_VERSION               	long_name         Data handbook version      
_FillValue                    6�   REFERENCE_DATE_TIME                 	long_name         !Date of reference for Julian days      conventions       YYYYMMDDHHMISS     
_FillValue                    6�   DATE_CREATION                   	long_name         Date of file creation      conventions       YYYYMMDDHHMISS     
_FillValue                    6�   DATE_UPDATE                 	long_name         Date of update of this file    conventions       YYYYMMDDHHMISS     
_FillValue                    6�   PLATFORM_NUMBER                   	long_name         Float unique identifier    conventions       WMO float identifier : A9IIIII     
_FillValue                    7   PROJECT_NAME                  	long_name         Name of the project    
_FillValue                  @  7   PI_NAME                   	long_name         "Name of the principal investigator     
_FillValue                  @  7T   STATION_PARAMETERS           	            	long_name         ,List of available parameters for the station   conventions       Argo reference table 3     
_FillValue                  0  7�   CYCLE_NUMBER               	long_name         Float cycle number     conventions       =0...N, 0 : launch cycle (if exists), 1 : first complete cycle      
_FillValue         ��        7�   	DIRECTION                  	long_name         !Direction of the station profiles      conventions       -A: ascending profiles, D: descending profiles      
_FillValue                    7�   DATA_CENTRE                   	long_name         .Data centre in charge of float data processing     conventions       Argo reference table 4     
_FillValue                    7�   DC_REFERENCE                  	long_name         (Station unique identifier in data centre   conventions       Data centre convention     
_FillValue                     7�   DATA_STATE_INDICATOR                  	long_name         1Degree of processing the data have passed through      conventions       Argo reference table 6     
_FillValue                    7�   	DATA_MODE                  	long_name         Delayed mode or real time data     conventions       >R : real time; D : delayed mode; A : real time with adjustment     
_FillValue                    7�   PLATFORM_TYPE                     	long_name         Type of float      conventions       Argo reference table 23    
_FillValue                     7�   FLOAT_SERIAL_NO                   	long_name         Serial number of the float     
_FillValue                     8   FIRMWARE_VERSION                  	long_name         Instrument firmware version    
_FillValue                     88   WMO_INST_TYPE                     	long_name         Coded instrument type      conventions       Argo reference table 8     
_FillValue                    8X   JULD               standard_name         time   	long_name         ?Julian day (UTC) of the station relative to REFERENCE_DATE_TIME    units         "days since 1950-01-01 00:00:00 UTC     conventions       8Relative julian days with decimal part (as parts of day)   
resolution        >�����h�   
_FillValue        A.�~       axis      T           8\   JULD_QC                	long_name         Quality on date and time   conventions       Argo reference table 2     
_FillValue                    8d   JULD_LOCATION                  	long_name         @Julian day (UTC) of the location relative to REFERENCE_DATE_TIME   units         "days since 1950-01-01 00:00:00 UTC     conventions       8Relative julian days with decimal part (as parts of day)   
resolution        >�����h�   
_FillValue        A.�~            8h   LATITUDE               	long_name         &Latitude of the station, best estimate     standard_name         latitude   units         degree_north   
_FillValue        @�i�       	valid_min         �V�        	valid_max         @V�        axis      Y           8p   	LONGITUDE                  	long_name         'Longitude of the station, best estimate    standard_name         	longitude      units         degree_east    
_FillValue        @�i�       	valid_min         �f�        	valid_max         @f�        axis      X           8x   POSITION_QC                	long_name         ,Quality on position (latitude and longitude)   conventions       Argo reference table 2     
_FillValue                    8�   POSITIONING_SYSTEM                    	long_name         Positioning system     
_FillValue                    8�   PROFILE_PRES_QC                	long_name         #Global quality flag of PRES profile    conventions       Argo reference table 2a    
_FillValue                    8�   PROFILE_TEMP_QC                	long_name         #Global quality flag of TEMP profile    conventions       Argo reference table 2a    
_FillValue                    8�   PROFILE_PSAL_QC                	long_name         #Global quality flag of PSAL profile    conventions       Argo reference table 2a    
_FillValue                    8�   VERTICAL_SAMPLING_SCHEME                  	long_name         Vertical sampling scheme   conventions       Argo reference table 16    
_FillValue                    8�   CONFIG_MISSION_NUMBER                  	long_name         :Unique number denoting the missions performed by the float     conventions       !1...N, 1 : first complete mission      
_FillValue         ��        9�   PRES         
      
   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���   axis      Z        �  9�   PRES_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  I4   PRES_ADJUSTED            
      	   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  M   PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  \�   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     
_FillValue        G�O�   	valid_min         �      	valid_max         B      C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  `�   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  p4   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     
_FillValue        G�O�   	valid_min         �      	valid_max         B      C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  t   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ��   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    
_FillValue        G�O�   	valid_min         @      	valid_max         B$     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  ��   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �4   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    
_FillValue        G�O�   	valid_min         @      	valid_max         B$     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  �   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ��   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         decibar    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  ��   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         degree_Celsius     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  �4   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  ��   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  �  �d   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                 	   ��   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                 	   ��   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                 	   ��   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  �  ��   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                    �t   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                    �x   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                    �|   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                    ��   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  @  ��   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                    ��   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                    ��   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    ��   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar         ��   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar         ��   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�        ��   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    ��Argo profile    3.1 1.2 19500101000000  20180815003512  20200115131518  4902363                                                                 JAMSTEC                                                         PRES            TEMP            PSAL              A   JA  I2_0576_270                     2C  D   NAVIS_A                         0576                            ARGO 092515                     863 @�y���P 1   @�y�M���@9�|�����d`ě��T1   GPS     A   B   B   Primary sampling: averaged [bin average for 1 Hz CTD]                                                                                                                                                                                                              @�  @�  A   A   A@  A`  A�  A�  A�  A�  A�  A�  A�  A�  B   B  B  B  B   B(  B0ffB8ffB?��BG��BP  BX  B`  Bh  Bp  Bx  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B���B�  B�  B�  B�  B�  B�  B�  C   C  C  C  C  C
  C  C  C  C  C  C  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D'��D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`y�Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dt� Du  Du� Dv  Dv� Dw  Dw� Dx  Dx� Dy  Dy� Dz  Dz� D{  D{� D|  D|� D}  D}� D~  D~� D  D� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�C3D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�<�D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�<�D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D D�� D�  D�@ DÀ D�� D�  D�@ DĀ D�� D�  D�@ Dŀ D�� D�  D�@ Dƀ D�� D�  D�@ Dǀ D�� D�  D�@ DȀ D�� D�  D�@ Dɀ D�� D�  D�@ Dʀ D�� D�  D�@ Dˀ D�� D�  D�@ D̀ D�� D�  D�@ D̀ D�� D�  D�@ D΀ D�� D�  D�@ Dπ D�� D�  D�@ DЀ D�� D�  D�@ Dр D�� D�  D�@ DҀ D�� D�  D�@ DӀ D�� D�  D�@ DԀ D�� D�  D�<�D�|�D�� D�  D�@ Dր D�� D�  D�@ D׃3D�� D�  D�@ D؀ D�� D�  D�@ Dـ D�� D�  D�@ Dڀ D�� D�  D�@ Dۀ D�� D�  D�@ D܀ D�� D�  D�@ D݀ D�� D�  D�@ Dހ D�� D�  D�@ D߀ D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�<�D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�C3D��fD��f11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  @~�R@�\)@�\)A�A?�A_�A�A��
A��
A��
A��
A��
A��
A��
A��
B�B�B�B�B'�B0Q�B8Q�B?�BG�BO�BW�B_�Bg�Bo�Bw�B�B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B�B���B���B���B���B���B���B���B���C��C��C��C��C	��C��C��C��C��C��C��C��C��C��C��C��C!��C#��C%��C'��C)��C+��C-��C/��C1��C3��C5��C7��C9��C;��C=��C?��CA��CC��CE��CG��CI��CK��CM��CO��CQ��CS��CU��CW��CY��C[��C]��C_��Ca��Cc��Ce��Cg��Ci��Ck��Cm��Co��Cq��Cs��Cu��Cw��Cy��C{��C}��C��C��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qD ~�D ��D~�D��D~�D��D~�D��D~�D��D~�D��D~�D��D~�D��D~�D��D	~�D	��D
~�D
��D~�D��D~�D��D~�D��D~�D��D~�D��D~�D��D~�D��D~�D��D~�D��D~�D��D~�D��D~�D��D~�D��D~�D��D~�D��D~�D��D~�D��D~�D��D~�D��D~�D��D~�D��D ~�D ��D!~�D!��D"~�D"��D#~�D#��D$~�D$��D%~�D%��D&~�D&��D'~�D'�RD(~�D(��D)~�D)��D*~�D*��D+~�D+��D,~�D,��D-~�D-��D.~�D.��D/~�D/��D0~�D0��D1~�D1��D2~�D2��D3~�D3��D4~�D4��D5~�D5��D6~�D6��D7~�D7��D8~�D8��D9~�D9��D:~�D:��D;~�D;��D<~�D<��D=~�D=��D>~�D>��D?~�D?��D@~�D@��DA~�DA��DB~�DB��DC~�DC��DD~�DD��DE~�DE��DF~�DF��DG~�DG��DH~�DH��DI~�DI��DJ~�DJ��DK~�DK��DL~�DL��DM~�DM��DN~�DN��DO~�DO��DP~�DP��DQ~�DQ��DR~�DR��DS~�DS��DT~�DT��DU~�DU��DV~�DV��DW~�DW��DX~�DX��DY~�DY��DZ~�DZ��D[~�D[��D\~�D\��D]~�D]��D^~�D^��D_~�D_��D`xRD`��Da~�Da��Db~�Db��Dc~�Dc��Dd~�Dd��De~�De��Df~�Df��Dg~�Dg��Dh~�Dh��Di~�Di��Dj~�Dj��Dk~�Dk��Dl~�Dl��Dm~�Dm��Dn~�Dn��Do~�Do��Dp~�Dp��Dq~�Dq��Dr~�Dr��Ds~�Ds��Dt~�Dt��Du~�Du��Dv~�Dv��Dw~�Dw��Dx~�Dx��Dy~�Dy��Dz~�Dz��D{~�D{��D|~�D|��D}~�D}��D~~�D~��D~�D��D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�B�D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�<)D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�<)D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D¿\D��\D�?\D�\Dÿ\D��\D�?\D�\DĿ\D��\D�?\D�\Dſ\D��\D�?\D�\Dƿ\D��\D�?\D�\Dǿ\D��\D�?\D�\Dȿ\D��\D�?\D�\Dɿ\D��\D�?\D�\Dʿ\D��\D�?\D�\D˿\D��\D�?\D�\D̿\D��\D�?\D�\DͿ\D��\D�?\D�\Dο\D��\D�?\D�\DϿ\D��\D�?\D�\Dп\D��\D�?\D�\Dѿ\D��\D�?\D�\Dҿ\D��\D�?\D�\Dӿ\D��\D�?\D�\DԿ\D��\D�<)D�|)Dտ\D��\D�?\D�\Dֿ\D��\D�?\Dׂ�D׿\D��\D�?\D�\Dؿ\D��\D�?\D�\Dٿ\D��\D�?\D�\Dڿ\D��\D�?\D�\Dۿ\D��\D�?\D�\Dܿ\D��\D�?\D�\Dݿ\D��\D�?\D�\D޿\D��\D�?\D�\D߿\D��\D�?\D�\D�\D��\D�?\D�\D�\D��\D�?\D�\D�\D��\D�?\D�\D�\D��\D�?\D�\D�\D��\D�?\D�\D�\D��\D�?\D�\D�\D��\D�?\D�\D�\D��\D�?\D�\D�\D��\D�<)D�\D�\D��\D�?\D�\D�\D��\D�?\D�\D�\D��\D�?\D�\D�\D��\D�?\D�\D��\D��\D�?\D�\D�\D��\D�?\D�\D�\D��\D�?\D�\D�\D��\D�?\D�\D�\D��\D�?\D�\D�\D��\D�?\D�\D�\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�B�D���D���11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  A���AήAΣ�AΗ�AΓuAΑhA΋DA·+A΅A΃A�~�A�v�A�p�A�+A͝�A�=qA�bA��yA�dZA˙�A�{A���A��A��A��A�K�A�bNA��PA�\)A��A�dZA���A�7LA��-A�M�A��mA�9XA���A��
A��A���A�+A��A�$�A���A��A���A��FA���A� �A��RA���A��hA���A�I�A���A�oA�M�A�  A�S�A��RA�G�A�A���A��DA��A�C�A�$�A��A��!A��DA�/A� �A���A��;A�C�A�E�A��jA�&�A��DA��
A�=qA��7A�jA�K�A��A�jA�x�A���A�~�A~v�A|Q�A{�
A{|�Ay%Aw��Au�;Au33AsAr$�ApJAnA�AlbNAk��Ak"�Aj��Ait�Ah�!Ag�
Af9XAc�Ab1A`��A`~�A`1A_ƨA_x�A^��A]��A]%A\��A\$�A[�mA[C�AV�yAU|�AT��AR�ARI�AQ��AQƨAQ�-AQx�AQ"�APr�AO��AO&�AN��AM?}AL�AK|�AKVAJ-AHZAG�AG33AF�/AFffAE��AE�AE\)AD�ADACƨAChsAB�HAA�wA@�HA@r�A?�A>(�A=`BA<bNA:�HA9A8��A8E�A7G�A6�/A6��A6n�A6A�A5S�A4ȴA4ffA3�A2$�A1
=A0�\A/�A.��A. �A-XA,n�A+�A*=qA)��A(��A(�A(�9A(�uA(I�A&�/A&A%dZA#��A"�9A �A��A?}A��A�A=qA-A~�AdZA�/A�+A �A�7A%A�!A�AVA(�AC�AAr�A?}A��A�AXAA
�!A	�TA�+A?}An�A�mAQ�AoAI�A�A�FAS�@�"�@��j@�o@�J@�bN@� �@���@��
@���@�C�@�M�@�dZ@�M�@��@���@�O�@�@�1@��@���@�Q�@��@�5?@�%@�F@��@��/@�j@��
@��@�hs@�X@�&�@�j@�dZ@޸R@���@�I�@�ff@؛�@��
@ו�@�o@Չ7@�1'@�O�@·+@ˍP@�v�@�V@�@�7L@��@Ȭ@�A�@ǥ�@��@�ff@��T@ŉ7@�G�@���@���@�&�@��@�v�@��@�(�@�  @���@�K�@�5?@���@��@�dZ@�V@���@�ff@�E�@�$�@��@�J@��@��T@���@�O�@�1'@��H@�hs@�1'@��
@���@��@���@���@���@��@��j@�r�@��@�C�@�E�@���@���@���@�r�@�bN@�A�@�(�@��;@�t�@���@�7L@�Q�@��P@�o@��@��@�1'@�1@��;@���@�l�@��@�ȴ@�~�@�^5@�E�@�-@�$�@���@��/@�Ĝ@��j@���@�j@��@��;@��@�\)@�"�@�~�@���@��7@�X@�7L@��`@��u@�j@�  @��P@�v�@���@��T@��h@�/@� �@���@�K�@���@��@�$�@�p�@�?}@��@��j@��@��
@��P@�"�@�n�@�{@��#@���@�?}@���@��@��@�t�@�S�@�33@�@���@���@�X@�/@���@�z�@�1'@��@�  @��F@�l�@�"�@���@�V@�M�@�^5@�ff@�n�@�n�@�^5@���@��-@�`B@�V@�1'@|�@l�@K�@
=@~��@~ff@~{@}O�@|��@|��@|�@|�@|�j@|I�@|1@{��@{�@z��@z��@z^5@zJ@y��@y�@x��@xQ�@x  @v�y@vv�@v5?@v@u�@u�T@u��@u�h@u`B@u�@t��@t�/@t�@tZ@s��@sdZ@q��@p�`@p�9@p�@pQ�@pb@o��@o;d@n��@nff@m��@m�@m?}@m?}@m�@mV@mV@l��@l�/@l�@lI�@lI�@l9X@l1@k�m@k�F@k33@k@ko@j�H@j��@j�\@j�\@jM�@i�^@i%@h��@h �@g�w@g+@fȴ@fE�@e�T@eO�@d�@dj@d9X@d(�@d�@c��@cƨ@c��@ct�@c33@b�@b�\@b�\@b-@a��@ahs@ahs@ax�@ax�@`��@`�9@`�u@_�;@_|�@_+@^��@^��@^ff@^5?@^{@]�h@\�j@\�@\1@[ƨ@[o@ZM�@Y�@Y�#@Y��@Y�^@Y��@Yhs@Y&�@X�9@X�@XbN@X �@WK�@V��@VE�@V@U@U�h@U?}@U/@UV@T�j@Tj@T9X@S��@S��@So@R�H@R�!@R~�@Rn�@R-@Q�@Q%@P��@P�@Pb@O��@O|�@OK�@O
=@N��@N{@M�-@M�h@M`B@L�j@LZ@K��@K�F@K��@K�@Kt�@Kt�@Kt�@KC�@J��@JM�@J�@I��@I��@I�7@H��@H�9@H��@Hr�@HbN@HA�@Hb@G��@G�w@G�@Gl�@F�y@F�R@Fff@F5?@E@E`B@E?}@EV@EV@EV@EV@D�@D��@Dj@D�@C�@B�@B��@BM�@A�@AX@@��@@�9@@��@@bN@@  @?\)@>�@>�+@>ff@>@=�-@=�@=`B@=O�@=/@<��@<��@<1@;��@;S�@;33@;"�@:�@:�\@:n�@:^5@9�@9x�@9hs@9&�@8�9@8bN@81'@7�;@7;d@6�y@6ff@6{@5@5�h@5�@4�/@4��@4��@4�@4��@4Z@3�
@3C�@3@3@2�H@2��@2�!@2n�@2�@1��@1��@0�9@0��@0�u@0�@0 �@0  @/�;@/��@/|�@/;d@.��@.��@.�@.��@.�+@.v�@.V@.@-�h@-O�@,�@,z�@,(�@+��@+��@+S�@+@*�H@*��@*^5@*J@)�^@)��@)�7@)hs@)G�@)&�@(��@(�`@(Ĝ@(�@(Q�@(b@'�@'�;@'�@'l�@';d@'
=@&�y@&ȴ@&��@&�+@&�+@&V@%�@%�h@%p�@%p�@%p�@%p�@%`B@%�@$��@$z�@$Z@$�@$1@#�m@#�
@#�@#dZ@#S�@#33@#@"��@"�!@"^5@!�@!X@ r�@ Q�@ 1'@�@��@�w@��@|�@\)@;d@�@�y@��@�+@ff@V@@p�@?}@�@V@�@�/@�j@�@�@�@��@Z@9X@9X@1@�F@t�@S�@"�@@�H@��@�\@^5@J@�@��@x�@X@7L@�@��@�9@�u@bN@ �@�@�;@��@\)@;d@+@+@
=@�y@�R@�+@ff@$�@��@�-@p�@?}@V@�@�j@z�@j@j@�@�
@ƨ@�F@��@�@"�@o@��@^5@-@��@��@��@hs@�@��@�9@r�@bN@1'@ �@  @�;@��@��@\)@�@��@�+@V@$�@�@��@@�@/@��@�/@�@�D@j@j@Z@�@��@C�@o@
��@
�!@
�\@
^5@
-@
�@
J@	�@	��@	��@	�7@	x�@	hs@	7L@	&�@	�@	�@	%@��@��@�@ �@b@  @�;@�w@��@;d@�y@ȴ@��@��@��@�+@E�@5?@��@�h@O�@��@��@�D@�D@�D@�D@�D@z�@j@Z@9X@��@�m@�
@ƨ@�F@�F@��@S�@33@@�H@��@��@�!@��@~�@n�@M�@M�@=q@�@�@�#@�^@x�@7L@%@�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111441111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  A���AήAΣ�AΗ�AΓuAΑhA΋DA·+A΅A΃A�~�A�v�A�p�A�+A͝�A�=qA�bA��yA�dZA˙�A�{A���A��A��A��A�K�A�bNA��PA�\)A��A�dZA���A�7LA��-A�M�A��mA�9XA���A��
A��A���A�+A��A�$�A���A��A���A��FA���A� �A��RA���A��hA���A�I�A���A�oA�M�A�  A�S�A��RA�G�A�A���A��DA��A�C�A�$�A��A��!A��DA�/A� �A���A��;A�C�A�E�A��jA�&�A��DA��
A�=qA��7A�jA�K�A��A�jA�x�A���A�~�A~v�A|Q�A{�
A{|�Ay%Aw��Au�;Au33AsAr$�ApJAnA�AlbNAk��Ak"�Aj��Ait�Ah�!Ag�
Af9XAc�Ab1A`��A`~�A`1A_ƨA_x�A^��A]��A]%A\��A\$�G�O�G�O�AV�yAU|�AT��AR�ARI�AQ��AQƨAQ�-AQx�AQ"�APr�AO��AO&�AN��AM?}AL�AK|�AKVAJ-AHZAG�AG33AF�/AFffAE��AE�AE\)AD�ADACƨAChsAB�HAA�wA@�HA@r�A?�A>(�A=`BA<bNA:�HA9A8��A8E�A7G�A6�/A6��A6n�A6A�A5S�A4ȴA4ffA3�A2$�A1
=A0�\A/�A.��A. �A-XA,n�A+�A*=qA)��A(��A(�A(�9A(�uA(I�A&�/A&A%dZA#��A"�9A �A��A?}A��A�A=qA-A~�AdZA�/A�+A �A�7A%A�!A�AVA(�AC�AAr�A?}A��A�AXAA
�!A	�TA�+A?}An�A�mAQ�AoAI�A�A�FAS�@�"�@��j@�o@�J@�bN@� �@���@��
@���@�C�@�M�@�dZ@�M�@��@���@�O�@�@�1@��@���@�Q�@��@�5?@�%@�F@��@��/@�j@��
@��@�hs@�X@�&�@�j@�dZ@޸R@���@�I�@�ff@؛�@��
@ו�@�o@Չ7@�1'@�O�@·+@ˍP@�v�@�V@�@�7L@��@Ȭ@�A�@ǥ�@��@�ff@��T@ŉ7@�G�@���@���@�&�@��@�v�@��@�(�@�  @���@�K�@�5?@���@��@�dZ@�V@���@�ff@�E�@�$�@��@�J@��@��T@���@�O�@�1'@��H@�hs@�1'@��
@���@��@���@���@���@��@��j@�r�@��@�C�@�E�@���@���@���@�r�@�bN@�A�@�(�@��;@�t�@���@�7L@�Q�@��P@�o@��@��@�1'@�1@��;@���@�l�@��@�ȴ@�~�@�^5@�E�@�-@�$�@���@��/@�Ĝ@��j@���@�j@��@��;@��@�\)@�"�@�~�@���@��7@�X@�7L@��`@��u@�j@�  @��P@�v�@���@��T@��h@�/@� �@���@�K�@���@��@�$�@�p�@�?}@��@��j@��@��
@��P@�"�@�n�@�{@��#@���@�?}@���@��@��@�t�@�S�@�33@�@���@���@�X@�/@���@�z�@�1'@��@�  @��F@�l�@�"�@���@�V@�M�@�^5@�ff@�n�@�n�@�^5@���@��-@�`B@�V@�1'@|�@l�@K�@
=@~��@~ff@~{@}O�@|��@|��@|�@|�@|�j@|I�@|1@{��@{�@z��@z��@z^5@zJ@y��@y�@x��@xQ�@x  @v�y@vv�@v5?@v@u�@u�T@u��@u�h@u`B@u�@t��@t�/@t�@tZ@s��@sdZ@q��@p�`@p�9@p�@pQ�@pb@o��@o;d@n��@nff@m��@m�@m?}@m?}@m�@mV@mV@l��@l�/@l�@lI�@lI�@l9X@l1@k�m@k�F@k33@k@ko@j�H@j��@j�\@j�\@jM�@i�^@i%@h��@h �@g�w@g+@fȴ@fE�@e�T@eO�@d�@dj@d9X@d(�@d�@c��@cƨ@c��@ct�@c33@b�@b�\@b�\@b-@a��@ahs@ahs@ax�@ax�@`��@`�9@`�u@_�;@_|�@_+@^��@^��@^ff@^5?@^{@]�h@\�j@\�@\1@[ƨ@[o@ZM�@Y�@Y�#@Y��@Y�^@Y��@Yhs@Y&�@X�9@X�@XbN@X �@WK�@V��@VE�@V@U@U�h@U?}@U/@UV@T�j@Tj@T9X@S��@S��@So@R�H@R�!@R~�@Rn�@R-@Q�@Q%@P��@P�@Pb@O��@O|�@OK�@O
=@N��@N{@M�-@M�h@M`B@L�j@LZ@K��@K�F@K��@K�@Kt�@Kt�@Kt�@KC�@J��@JM�@J�@I��@I��@I�7@H��@H�9@H��@Hr�@HbN@HA�@Hb@G��@G�w@G�@Gl�@F�y@F�R@Fff@F5?@E@E`B@E?}@EV@EV@EV@EV@D�@D��@Dj@D�@C�@B�@B��@BM�@A�@AX@@��@@�9@@��@@bN@@  @?\)@>�@>�+@>ff@>@=�-@=�@=`B@=O�@=/@<��@<��@<1@;��@;S�@;33@;"�@:�@:�\@:n�@:^5@9�@9x�@9hs@9&�@8�9@8bN@81'@7�;@7;d@6�y@6ff@6{@5@5�h@5�@4�/@4��@4��@4�@4��@4Z@3�
@3C�@3@3@2�H@2��@2�!@2n�@2�@1��@1��@0�9@0��@0�u@0�@0 �@0  @/�;@/��@/|�@/;d@.��@.��@.�@.��@.�+@.v�@.V@.@-�h@-O�@,�@,z�@,(�@+��@+��@+S�@+@*�H@*��@*^5@*J@)�^@)��@)�7@)hs@)G�@)&�@(��@(�`@(Ĝ@(�@(Q�@(b@'�@'�;@'�@'l�@';d@'
=@&�y@&ȴ@&��@&�+@&�+@&V@%�@%�h@%p�@%p�@%p�@%p�@%`B@%�@$��@$z�@$Z@$�@$1@#�m@#�
@#�@#dZ@#S�@#33@#@"��@"�!@"^5@!�@!X@ r�@ Q�@ 1'@�@��@�w@��@|�@\)@;d@�@�y@��@�+@ff@V@@p�@?}@�@V@�@�/@�j@�@�@�@��@Z@9X@9X@1@�F@t�@S�@"�@@�H@��@�\@^5@J@�@��@x�@X@7L@�@��@�9@�u@bN@ �@�@�;@��@\)@;d@+@+@
=@�y@�R@�+@ff@$�@��@�-@p�@?}@V@�@�j@z�@j@j@�@�
@ƨ@�F@��@�@"�@o@��@^5@-@��@��@��@hs@�@��@�9@r�@bN@1'@ �@  @�;@��@��@\)@�@��@�+@V@$�@�@��@@�@/@��@�/@�@�D@j@j@Z@�@��@C�@o@
��@
�!@
�\@
^5@
-@
�@
J@	�@	��@	��@	�7@	x�@	hs@	7L@	&�@	�@	�@	%@��@��@�@ �@b@  @�;@�w@��@;d@�y@ȴ@��@��@��@�+@E�@5?@��@�h@O�@��@��@�D@�D@�D@�D@�D@z�@j@Z@9X@��@�m@�
@ƨ@�F@�F@��@S�@33@@�H@��@��@�!@��@~�@n�@M�@M�@=q@�@�@�#@�^@x�@7L@%@�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111441111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  BiyBiyBjBjBjBjBjBjBjBiyBhsBgmBbNBYB]/B`BB\)BQ�BB�BE�B0!B9XB�B��BbBN�B�7B� Bs�B}�Bn�BaHB}�B}�Bv�BjB\)BA�B�B?}B9XB7LB:^B>wB=qB;dB7LB2-B"�BuB��B��B�XB�?B�wB�9B�-B�!B��B�+B� B|�B_;BW
Be`BR�B:^B:^B8RB8RB+BPB1B
��B
��B
��B
�B
ȴB
��B
�LB
�B
��B
�-B
�B
��B
�oB
{�B
}�B
z�B
cTB
P�B
`BB
[#B
A�B
49B
/B
+B
�B
hB	��B	��B	�B	�B	�B	�yB	�B	��B	ȴB	�LB	��B	��B	�!B	�?B	�?B	�-B	�B	��B	��B	��B	��B	�hB	�7B	u�B	A�B	]/B	jB	_;B	l�B	p�B	p�B	o�B	jB	cTB	XB	Q�B	O�B	I�B	>wB	6FB	:^B	6FB	/B	�B	-B	33B	2-B	-B	,B	,B	(�B	"�B	�B	�B	�B	hB	B��B��B�B�ZB�`B�B��B��B��B��B��B��B��B��B��BĜBB��B�B�9B�B�3B�-B��B��B��B��B��B�{B��B��B��B��B��B��B�7B�B�1Bv�Bq�BjBs�By�Bz�Bs�Bn�BS�BO�B_;BgmBhsBe`BaHB_;B_;BW
BK�BJ�BE�B>wB6FB;dBC�BA�B>wB0!B.B6FB.B/B1'B5?B"�B#�B/B2-B2-B&�B�B�B�B&�B!�B0!B0!B/B,B'�B�BoB �B(�B)�B%�B!�B �B�BuB�B�B�B�BuBoB�B�B�BuB�B"�B�B�BoB�BhBPB	7BJB�B�B�BbB1BB��B%B�B!�B�B�B �B �B�B�B�B#�B&�B(�B(�B#�B�B�B"�B$�B%�B&�B33B5?B0!B,B-B-B#�B#�B/B9XBG�BH�BI�BH�BG�BF�BC�B?}B8RB6FB:^BC�BO�BP�BP�BI�BN�BT�BQ�BT�BT�BS�BR�BR�BXBhsBiyBiyBk�BjBiyBffBdZB]/BgmBk�Bm�Bq�Bn�Br�B�B�+B�1B�+B�1B�1B�PB�PB�bB�{B��B��B�oB�oB��B��B��B��B��B��B��B��B��B��B��B�-B�3B�9B�3B�?B�LB�FB�FB�FB�}BƨBĜBĜBB��B��B��B��B��B��B�#B�#B�B�B�HB�NB�TB�ZB�B�B�B�B�B��B��B	  B	B	B	B	  B��B	DB	\B	\B	\B	�B	�B	�B	�B	�B	!�B	 �B	%�B	)�B	-B	.B	.B	.B	,B	)�B	+B	-B	0!B	0!B	9XB	A�B	B�B	B�B	C�B	E�B	E�B	F�B	J�B	R�B	S�B	T�B	S�B	R�B	T�B	VB	T�B	W
B	^5B	_;B	`BB	bNB	cTB	hsB	gmB	l�B	k�B	r�B	u�B	w�B	y�B	y�B	y�B	{�B	{�B	|�B	~�B	� B	� B	� B	� B	�B	�B	�=B	�uB	�{B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�B	�B	�!B	�'B	�3B	�9B	�FB	�?B	�9B	�3B	�FB	�RB	�XB	�jB	�qB	�}B	��B	B	B	ŢB	ƨB	ɺB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	��B	�
B	�B	�B	�B	�#B	�)B	�#B	�B	�B	�/B	�HB	�BB	�;B	�HB	�`B	�sB	�sB	�sB	�sB	�mB	�mB	�mB	�yB	�B	�yB	�mB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
  B
B
B	��B
B
B
B
%B
+B
+B
+B
%B
B
B
%B
1B
1B
1B
1B
1B

=B
JB
JB
JB
JB
JB
JB
PB
PB
JB
DB
VB
PB
VB
VB
\B
hB
oB
oB
uB
uB
hB
hB
bB
hB
bB
hB
uB
{B
{B
{B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
!�B
"�B
"�B
"�B
!�B
"�B
#�B
"�B
"�B
%�B
$�B
$�B
$�B
%�B
%�B
$�B
&�B
&�B
'�B
(�B
)�B
(�B
+B
-B
-B
,B
,B
+B
+B
+B
.B
/B
/B
/B
/B
.B
.B
.B
/B
-B
1'B
2-B
1'B
0!B
2-B
2-B
2-B
33B
33B
33B
49B
49B
49B
49B
5?B
49B
33B
33B
49B
49B
49B
5?B
6FB
6FB
7LB
7LB
8RB
8RB
8RB
8RB
9XB
;dB
;dB
;dB
;dB
;dB
;dB
<jB
<jB
;dB
<jB
<jB
=qB
=qB
=qB
=qB
>wB
>wB
?}B
?}B
?}B
?}B
?}B
>wB
>wB
?}B
A�B
B�B
B�B
A�B
A�B
@�B
@�B
@�B
B�B
A�B
C�B
C�B
C�B
B�B
C�B
D�B
D�B
C�B
D�B
D�B
C�B
B�B
C�B
B�B
H�B
H�B
H�B
I�B
I�B
I�B
I�B
J�B
J�B
J�B
J�B
I�B
J�B
K�B
K�B
J�B
I�B
L�B
M�B
M�B
M�B
M�B
M�B
N�B
N�B
N�B
N�B
M�B
N�B
N�B
N�B
M�B
N�B
O�B
O�B
O�B
O�B
O�B
O�B
O�B
O�B
P�B
P�B
Q�B
Q�B
Q�B
Q�B
Q�B
R�B
R�B
R�B
R�B
S�B
S�B
S�B
S�B
T�B
VB
VB
T�B
T�B
T�B
T�B
VB
T�B
T�B
W
B
W
B
W
B
W
B
XB
XB
XB
YB
YB
XB
XB
ZB
ZB
ZB
YB
YB
ZB
YB
YB
[#B
[#B
[#B
[#B
\)B
[#B
]/B
^5B
]/B
_;B
_;B
_;B
_;B
_;B
_;B
^5B
^5B
]/B
_;B
aHB
`BB
`BB
aHB
aHB
aHB
aHB
`BB
bNB
bNB
cTB
cTB
dZB
dZB
cTB
bNB
aHB
cTB
dZB
dZB
e`B
ffB
ffB
e`B
gmB
gmB
ffB
ffB
hsB
hsB
hsB
hsB
gmB
hsB
iyB
iyB
hsB
hsB
gmB
gmB
gmB
iyB
jB
iyB
iyB
iyB
hsB
iyB
jB
k�B
k�B
l�B
k�B
jB
k�B
jB
k�B
k�B
k�B
m�B
n�B
o�B
o�B
o�B
o�B
o�B
n�B
n�B
n�B
n�B
o�B
o�B
o�B
o�B
o�B
o�B
n�B
o�B
o�B
p�B
q�B
q�B
q�B
q�B
p�B
q�B
q�B
q�B
q�B
q�B
q�B
r�B
r�B
q�B
r�B
s�B
t�B
u�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111441111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  Bi�Bi�Bj�BjBjBjBjBjBjBi�Bh�Bg�BcBZ7B]�B`�B\�BS@BD�BG�B6�BBAB$�B�5B	BT,B�)B��Bv�B� BrBd�B~�B~�BxBl=B^OBE�B"NBA B;B8�B;B>�B=�B;�B7�B2�B$&B�B��BѷB��B�B��B��B��B�'B�/B��B�UB~wBa�BYBffBUB<�B<jB9>B8�B,=B�B	�B
��B
֡B
�B
�?B
�=B
�B
��B
��B
�eB
�aB
�wB
��B
�FB
~BB
}B
|6B
f2B
S[B
`�B
\)B
D3B
6B
1B
,"B
!�B
�B	��B	��B	�B	�B	�aB	�KB	ڠB	�B	�#B	��B	��B	��B	�AB	��B	��B	��B	��B	��B	�	B	��B	�B	�:B	��G�O�G�O�B	^�B	k�B	aHB	m)B	qB	qB	o�B	j�B	c�B	X�B	R�B	P�B	J�B	@B	7�B	;0B	7B	0oB	�B	-�B	3�B	2�B	-�B	,�B	,qB	)yB	#�B	�B	!B	=B	TB	�B�.B��B�GB��B�BۦB��B�4B��B��B��B�oB�MB�aB�hB��B�aB�UB��B�tB��B��B�3B�ZB��B��B��B��B�B�B�]B�B�RB�@B�dB�B�YB�RBx�Bs�Bl�Bt�Bz�B{dBt�Bo�BV�BR B`�Bh$BiBfBbNB`B_�BX+BM6BLBG+B@OB8lB<�BD�BB�B?�B2B/�B7�B0!B0�B2aB6FB$�B%`B0B2�B2�B(
B�B1B �B'�B"�B0;B0UB/OB,qB(�B�B,B!|B)DB*0B&�B"hB!bB�B�BCBkBeB_B{B�B+BB7B�B/B"�B B#B[BB:B�B
�BPBB�B?B�B	lB�B��B�B9B!�B 'BVB!B!B!BIBIB$@B'RB)DB)_B$�BB�B#�B%�B&�B'�B3MB5tB0�B,�B-�B-�B%,B%zB0!B:*BG�BH�BI�BH�BG�BF�BC�B@B9rB7fB;BDgBPBQ4BQBJrBO\BUMBR�BUMBUMBT{BS�BS�BX�Bh�Bi�Bi�Bk�Bj�Bi�Bf�Bd�B^5Bh
Bl=Bn/Br-Bo�Bs�B�UB�_B�fB�zB��B��B��B��B��B��B��B��B��B�&B��B��B��B��B�B��B��B� B�&B�tB�zB�GB�hB�nB��B��B��B��B��B��B��B��B�B�B�GB�)B�B�:B�[B�hB�oB�WB�WBچBڠB�|B�B�B��B��B��B��B�B�B�?B�JB	 4B	AB	[B	UB	 �B��B	xB	�B	�B	�B	�B	�B	�B	�B	!B	"B	!-B	&B	*B	-B	.B	.B	.IB	,=B	*KB	+QB	-]B	0�B	0�B	9�B	A�B	B�B	B�B	C�B	E�B	E�B	F�B	J�B	R�B	S�B	T�B	TB	S&B	UB	VB	U2B	WYB	^5B	_VB	`�B	b�B	c�B	h�B	g�B	l�B	k�B	r�B	u�B	w�B	y�B	y�B	zB	|B	|B	}B	B	�B	�B	�4B	�OB	�MB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�
B	�
B	�*B	�"B	�)B	�"B	�)B	�)B	�IB	�;B	�'B	�hB	�TB	�FB	�tB	�nB	��B	�zB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	� B	�,B	��B	��B	�B	�&B	�B	�B	�aB	�$B	�EB	�7B	�eB	�=B	�]B	�=B	�QB	�kB	�dB	�HB	�vB	ߤB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	�B	��B	�B	�B	��B	��B	��B	��B	��B	��B	��B	�B	��B	�B	�B	��B	��B	��B	�B	��B	�B	�B	�6B	�B	�B	�"B	�"B	�(B
 B
 B
 B	�HB
AB
GB
9B
YB
+B
EB
+B
?B
9B
MB
YB
KB
KB
KB
KB
fB

XB
JB
dB
dB
~B
dB
~B
jB
jB
dB
xB
pB
jB
pB
�B
vB
�B
�B
oB
uB
uB
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
!�B
"�B
"�B
"�B
"B
#B
#�B
#B
#B
%�B
$�B
%B
$�B
&B
&B
%B
'B
'B
(
B
)B
*B
)*B
+6B
-B
-B
,"B
,"B
+6B
+QB
+6B
./B
/B
/5B
/5B
/5B
./B
.IB
./B
/5B
-wB
1'B
2-B
1[B
0;B
2GB
2GB
2GB
3hB
3MB
3MB
49B
4nB
4TB
4TB
5ZB
4TB
3hB
3hB
4TB
4nB
4�B
5ZB
6`B
6`B
7fB
7fB
8lB
8lB
8�B
8�B
9rB
;B
;B
;B
;B
;B
;B
<�B
<�B
;B
<�B
<�B
=�B
=�B
=�B
=�B
>�B
>�B
?�B
?�B
?�B
?}B
?�B
>�B
>�B
?�B
A�B
B�B
B�B
A�B
A�B
@�B
@�B
@�B
B�B
A�B
C�B
C�B
C�B
B�B
C�B
D�B
D�B
C�B
D�B
D�B
C�B
B�B
C�B
B�B
H�B
H�B
H�B
I�B
I�B
I�B
I�B
J�B
J�B
J�B
J�B
I�B
J�B
K�B
K�B
J�B
J	B
L�B
M�B
M�B
M�B
M�B
NB
N�B
N�B
N�B
OB
M�B
N�B
N�B
N�B
M�B
N�B
O�B
O�B
O�B
O�B
O�B
PB
PB
PB
Q B
Q B
R B
R B
RB
RB
RB
SB
SB
SB
SB
TB
TB
TB
TB
UB
VB
VB
UB
UB
U2B
U2B
VB
UB
UB
W$B
W$B
W$B
W$B
X+B
X+B
X+B
YB
Y1B
XEB
X+B
ZB
ZB
Z7B
Y1B
Y1B
ZB
Y1B
YKB
[=B
[=B
[=B
[=B
\CB
[=B
]IB
^OB
]IB
_VB
_VB
_;B
_VB
_VB
_VB
^OB
^OB
]dB
_VB
abB
`\B
`\B
abB
abB
abB
abB
`\B
bhB
bhB
cnB
cnB
dtB
dZB
c�B
bhB
a|B
cnB
d�B
d�B
ezB
f�B
f�B
ezB
gmB
gmB
f�B
f�B
hsB
hsB
hsB
hsB
g�B
hsB
iyB
iyB
h�B
h�B
g�B
g�B
g�B
iyB
jB
i�B
i�B
i�B
h�B
i�B
j�B
k�B
k�B
l�B
k�B
j�B
k�B
j�B
k�B
k�B
k�B
m�B
n�B
o�B
o�B
o�B
o�B
o�B
n�B
n�B
n�B
n�B
o�B
o�B
o�B
o�B
o�B
o�B
n�B
o�B
o�B
p�B
q�B
q�B
q�B
q�B
p�B
q�B
q�B
q�B
q�B
q�B
q�B
r�B
r�B
q�B
r�B
s�B
t�B
u�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111441111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  @��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oG�O�G�O�;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<Q�<�C�<*d�<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
G�O�G�O�<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
PRES            TEMP            PSAL            PRES            TEMP            PSAL            PRES            TEMP            PSAL            PRES_ADJ=PRES-SP,  where SP is SURFACE PRESSURE from next cycle; PRES_ADJ_ERR=Manufacturer sensor accuracy                                                                                                                                                      TEMP_ADJ=TEMP; TEMP_ADJ_ERR=Manufacturer sensor accuracy                                                                                                                                                                                                        PSAL_ADJ = RecalS= psal(PRES_ADJ,TEMP,Conductivity)                                                                                                                                                                                                             None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            PSAL_ADJ = celltm_sbe41(RecalS,TEMP,P,elapsed_time,alpha,tau); elapsed_time=P/mean_rise_rate; P=dbar since the start of the profile for each samples                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            PSAL_ADJ_ERR=max(RecalS & CTM error, 0.01(PSS-78))                                                                                                                                                                                                              SP=0.02(dbar)                                                                                                                                                                                                                                                   None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            alpha=0.0267C, tau=18.6s, mean_rise_rate = 0.09 dbar/second                                                                                                                                                                                                     None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            Pressure Correction using reported SURFACE PRESSURE                                                                                                                                                                                                             None                                                                                                                                                                                                                                                            Salinity Recalculation using PRES_ADJ                                                                                                                                                                                                                           None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            Correction for conductivity cell thermal mass error(CTM), Johnson et al., 2007, JAOT                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            No adjustment is needed                                                                                                                                                                                                                                         201808190042582018081900425820180819004258201808190200182018081902001820180819020018201808200030072018082000300720180820003007  JA  ARFMdecpA19c                                                                20180815093511  IP                  G�O�G�O�G�O�                JA  ARFMfmtp2.8                                                                 20180815003512  IP                  G�O�G�O�G�O�                JA  ARCAsspa3.1b                                                                20180815003516  IP  PRES            G�O�G�O�G�O�                JA  ARGQrqcppo_c                                                                20180815003516  QCP$                G�O�G�O�G�O�              3CJA  ARGQrqcpt19d                                                                20180815003517  QCP$                G�O�G�O�G�O�           80000JA  ARGQaqcpt19d                                                                20180815003517  QCP$                G�O�G�O�G�O�           80000JA  ARGQrqcp2.8e                                                                20180815003517  QCP$                G�O�G�O�G�O�            FB40JA  ARGQrqcp2.8e                                                                20180815003517  QCF$                G�O�G�O�G�O�            4000JA  ARGQaqcp2.8e                                                                20180815003517  QCP$                G�O�G�O�G�O�            FB40JA  ARGQaqcp2.8e                                                                20180815003517  QCF$                G�O�G�O�G�O�            4000JA  ARGQrqcpt16c                                                                20180815003517  QCP$                G�O�G�O�G�O�           10000JA      jafc1.0                                                                 20180815003518                      G�O�G�O�G�O�                JA  ARUP                                                                        20180815005542                      G�O�G�O�G�O�                JM  ARGQJMQC2.0                                                                 20180815153357  CV  JULD            G�O�G�O�F�ͯ                JM  ARCAJMQC2.0                                                                 20180818154258  CV  PRES_ADJUSTED   G�O�G�O�G�O�                JM  ARCAJMQC2.0                                                                 20180818154258  CV  PSAL_ADJUSTED   G�O�G�O�G�O�                JM  ARCAJMTM1.0                                                                 20180818170018  CV  PSAL_ADJUSTED   G�O�G�O�G�O�                JM  ARSQOW  1.1 2017V1                                                          20180819153007  IP  PSAL_ADJUSTED   G�O�G�O�G�O�                JA  ARDU                                                                        20200115131518                      G�O�G�O�G�O�                