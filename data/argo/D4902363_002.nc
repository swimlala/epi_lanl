CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       JMA    source        
Argo float     history       b2016-05-23T06:35:29Z creation;2016-05-23T06:35:31Z conversion to V3.1;2019-12-19T08:39:32Z update;     
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
_FillValue                 �  IH   PRES_ADJUSTED            
      	   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  M4   PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  \�   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     
_FillValue        G�O�   	valid_min         �      	valid_max         B      C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  `�   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  px   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     
_FillValue        G�O�   	valid_min         �      	valid_max         B      C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  td   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    
_FillValue        G�O�   	valid_min         @      	valid_max         B$     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  ��   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ��   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    
_FillValue        G�O�   	valid_min         @      	valid_max         B$     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  ��   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �@   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         decibar    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  �,   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         degree_Celsius     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  ��   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  ΄   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  �  �0   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                 	   ��   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                 	   ��   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                 	   ��   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  �  ��   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                    �@   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                    �D   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                    �H   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                    �L   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  @  �P   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                    ��   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                    ��   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    ��   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar         ��   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar         ��   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�        ��   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    ��Argo profile    3.1 1.2 19500101000000  20160523063529  20200115101516  4902363                                                                 JAMSTEC                                                         PRES            TEMP            PSAL               A   JA  I2_0576_002                     2C  D   NAVIS_A                         0576                            ARGO 092515                     863 @׭. m: 1   @׭.�b��@;����?�dR����1   GPS     A   A   A   Primary sampling: averaged [bin average for 1 Hz CTD]                                                                                                                                                                                                              @�  @�  A   A   A@  A`  A�  A�  A�  A�  A�  A�  A�  A�  B   B  B  B  B   B(  B0  B8  B@  BHffBP  BXffB`  Bh  Bp  Bx  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  C   C  C  C  C  C
  C  C  C  C  C  C  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D��D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D y�D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dt� Du  Du� Dv  Dv� Dw  Dw� Dx  Dx� Dy  Dy� Dz  Dz� D{  D{� D|  D|� D}  D}� D~  D~� D  D� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�C3D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D��3D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D���D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D D�� D�  D�@ DÀ D�� D�  D�@ DĀ D�� D�  D�@ Dŀ D�� D�  D�@ Dƀ D�� D�  D�@ Dǀ D�� D�  D�@ DȀ D�� D�  D�@ Dɀ D�� D�  D�@ Dʀ D�� D�  D�@ Dˀ D�� D�  D�@ D̀ D�� D�  D�@ D̀ D�� D�  D�@ D΀ D�� D�  D�@ Dπ D�� D�  D�@ DЀ D�� D�  D�@ Dр D�� D�  D�@ DҀ D�� D�  D�@ DӀ D�� D�  D�@ DԀ D�� D�  D�@ DՀ D�� D�  D�@ Dր D�� D�  D�@ D׀ D�� D�  D�@ D؀ D�� D�  D�@ Dـ D�� D�  D�@ Dڀ D�� D�  D�@ Dۀ D�� D�  D�@ D܀ D�� D�  D�@ D݀ D�� D�  D�@ Dހ D�� D�  D�@ D߀ D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�3D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D��3D���1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 @�\)@�\)@�\)A�A?�A_�A�A��
A��
A��
A��
A��
A��
A��
A��
B�B�B�B�B'�B/�B7�B?�BHQ�BO�BXQ�B_�Bg�Bo�Bw�B�B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���C��C��C��C��C	��C��C��C��C��C��C��C��C��C��C��C��C!��C#��C%��C'��C)��C+��C-��C/��C1��C3��C5��C7��C9��C;��C=��C?��CA��CC��CE��CG��CI��CK��CM��CO��CQ��CS��CU��CW��CY��C[��C]��C_��Ca��Cc��Ce��Cg��Ci��Ck��Cm��Co��Cq��Cs��Cu��Cw��Cy��C{��C}��C��C��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qD ~�D ��D~�D��D~�D��D~�D��D~�D�RD~�D��D~�D��D~�D��D~�D��D	~�D	��D
~�D
��D~�D��D~�D��D~�D��D~�D��D~�D��D~�D��D~�D��D~�D��D~�D��D~�D��D~�D��D~�D��D~�D��D~�D��D~�D��D~�D��D~�D��D~�D��D~�D��D~�D��D~�D��D xRD ��D!~�D!��D"~�D"��D#~�D#��D$~�D$��D%~�D%��D&~�D&��D'~�D'��D(~�D(��D)~�D)��D*~�D*��D+~�D+��D,~�D,��D-~�D-��D.~�D.��D/~�D/��D0~�D0��D1~�D1��D2~�D2��D3~�D3��D4~�D4��D5~�D5��D6~�D6��D7~�D7��D8~�D8��D9~�D9��D:~�D:��D;~�D;��D<~�D<��D=~�D=��D>~�D>��D?~�D?��D@~�D@��DA~�DA��DB~�DB��DC~�DC��DD~�DD��DE~�DE��DF~�DF��DG~�DG��DH~�DH��DI~�DI��DJ~�DJ��DK~�DK��DL~�DL��DM~�DM��DN~�DN��DO~�DO��DP~�DP��DQ~�DQ��DR~�DR��DS~�DS��DT~�DT��DU~�DU��DV~�DV��DW~�DW��DX~�DX��DY~�DY��DZ~�DZ��D[~�D[��D\~�D\��D]~�D]��D^~�D^��D_~�D_��D`~�D`��Da~�Da��Db~�Db��Dc~�Dc��Dd~�Dd��De~�De��Df~�Df��Dg~�Dg��Dh~�Dh��Di~�Di��Dj~�Dj��Dk~�Dk��Dl~�Dl��Dm~�Dm��Dn~�Dn��Do~�Do��Dp~�Dp��Dq~�Dq��Dr~�Dr��Ds~�Ds��Dt~�Dt��Du~�Du��Dv~�Dv��Dw~�Dw��Dx~�Dx��Dy~�Dy��Dz~�Dz��D{~�D{��D|~�D|��D}~�D}��D~~�D~��D~�D��D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�B�D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D���D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��)D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D¿\D��\D�?\D�\Dÿ\D��\D�?\D�\DĿ\D��\D�?\D�\Dſ\D��\D�?\D�\Dƿ\D��\D�?\D�\Dǿ\D��\D�?\D�\Dȿ\D��\D�?\D�\Dɿ\D��\D�?\D�\Dʿ\D��\D�?\D�\D˿\D��\D�?\D�\D̿\D��\D�?\D�\DͿ\D��\D�?\D�\Dο\D��\D�?\D�\DϿ\D��\D�?\D�\Dп\D��\D�?\D�\Dѿ\D��\D�?\D�\Dҿ\D��\D�?\D�\Dӿ\D��\D�?\D�\DԿ\D��\D�?\D�\Dտ\D��\D�?\D�\Dֿ\D��\D�?\D�\D׿\D��\D�?\D�\Dؿ\D��\D�?\D�\Dٿ\D��\D�?\D�\Dڿ\D��\D�?\D�\Dۿ\D��\D�?\D�\Dܿ\D��\D�?\D�\Dݿ\D��\D�?\D�\D޿\D��\D�?\D�\D߿\D��\D�?\D�\D�\D��\D�?\D�\D�\D��\D�?\D�\D�\D��\D�?\D�\D�\D��\D�?\D�\D�\D��\D�?\D�\D�\D��\D�?\D�\D�\D��\D�?\D�\D�\D��\D�?\D�\D�\D��\D�?\D�\D�\D��\D�?\D�\D�\D��\D�?\D�\D�\D��\D�?\D�\D�\D��\D�?\D�\D��\D��\D�?\D�\D�\D��\D�?\D�\D�\D��\D�?\D�\D�\D��\D�?\D�\D�\D��D�?\D�\D�\D��\D�?\D�\D�\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D�D��)1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 A�r�A�p�A�n�A�n�A�r�A�r�A�p�A�n�A�dZA�$�A�hsA��A�A��A��/A���A���A�C�A���A�\)A��/A�r�A���A��!A�^5A�A��\A��A�z�A���A��A���A���A�ƨA��A�`BA�~�A���A�"�A�p�A�K�A��A���A�;dA�l�A���A��+A�S�A�z�A�ȴA�A��DA��A�~�A�VA���A�I�A��A�=qA��mA�jA��hA���A�A���A��mA��A��yA���A��#A��hA�l�A��#A�(�A��-A�  A�?}A���A�t�A�;dA��A���A��TA��!A�t�A�A��RA�Q�A�E�A���A���A��hA�+A�ĜA�G�A���A��A�S�A���A���A��yA�S�A�JA��mA��PA�;dA���A�&�A�hsA�n�A�dZA��A��yA�z�A�-A�A�A��A��^A�%A�ffA��^A���A�I�A�G�A��`A�(�A���A�%A~1A|�\Az{Ax5?Aw��Au�7As
=Aq�Ap��Ao�AoVAn�9An5?Am��Aln�Ak��Aj�!Ah�yAh �Ag�TAg��Af�Af��Ae�AcO�AbA�AbbAa�AaAax�Aa�A`z�A`5?A_�A^ĜA^�A]�wA\��A[�A[hsAZz�AYdZAW�AV�AUl�AU33AT��AS�ASp�AS�AR��ARQ�AQ�7AP�AO�#AN��AM�AK�AK�AJA�AI�mAIK�AH��AH-AGS�AF5?AE|�AEhsAE33AD�AC�ABr�AA��A@�uA>�A=A;�mA;�hA:�yA9�A9`BA9
=A8�9A89XA7dZA6�uA2��A133A/�;A.��A.I�A*v�A(n�A'A'��A'l�A'?}A&��A&��A&1'A%�PA%
=A$=qA"r�A��A�DAA��A�AE�A�A��AdZA/AA��A�wAG�A��AA�A �AA|�A��AĜAbNA��Al�A��A��A�AjA�A"�A	��A��AffA�-A��A�9A$�A��AoA�Az�AoA ffA E�A -A b@��w@��\@��@�E�@���@�1'@��
@���@�~�@��@�&�@�K�@�E�@�7@��/@�b@@�9X@�^5@�r�@�
=@�bN@��;@㕁@�@�x�@�&�@�;d@ܼj@�
=@�n�@�E�@�V@ى7@�Ĝ@��@Ձ@���@�r�@��m@��@Ь@�l�@�+@���@·+@�$�@̼j@���@ʟ�@ɡ�@�  @���@���@�G�@�j@�@��m@�J@�r�@��P@��j@�E�@��@��-@���@�@�n�@��T@��h@��@�Q�@���@�?}@��@��m@�|�@��y@�@���@��@���@��+@�%@���@�l�@�+@���@�=q@��h@�%@�z�@��m@��P@��@��@�ȴ@���@�n�@�=q@���@���@���@�7L@��/@���@�33@�@���@���@��@��/@�9X@�\)@�@��y@��+@�^5@�M�@�$�@���@�V@�1@�33@��+@�@�p�@��@��@�\)@���@�n�@�E�@�$�@�J@��h@��@�j@�1'@�1@��;@��w@���@��P@�dZ@�;d@��@�
=@��y@�ȴ@��\@�M�@��^@���@�1'@�  @��
@�33@��@�@���@��h@�p�@�7L@���@���@��u@��u@��u@���@��u@��u@��D@��@�j@�Q�@��@��@��
@���@��@�o@��+@�=q@���@�p�@�G�@�G�@�&�@�V@���@��D@��@�Z@�A�@�b@l�@~�y@}@}V@|�/@|I�@|1@{�m@{��@{�@z�@y��@yx�@x�u@xA�@w�;@w|�@wl�@wl�@wl�@w\)@w;d@w;d@w�@v�+@u��@u?}@u�@uV@t��@t�@t�D@t�D@tI�@s��@sdZ@sS�@sS�@s33@r�!@rJ@q��@q�#@q�#@q��@q��@q%@o��@oK�@n�@nE�@m�h@m?}@k�m@k��@kt�@kC�@j��@j�!@j-@ix�@h�`@hbN@hA�@h1'@h �@h �@hb@g�@g�;@g��@gl�@g;d@g
=@fv�@fff@e�@e��@ep�@e/@d��@cƨ@c�@cS�@cC�@co@b^5@a�^@a�7@a7L@`�`@`�9@`��@`�u@`A�@_�@_|�@_K�@_;d@_�@_
=@^��@^�y@^��@^�y@^�y@^�@^��@^��@^�+@^ff@]��@]`B@]/@]V@\��@\��@\�D@\j@\Z@\9X@\(�@\�@[�
@[S�@[o@Z�H@Z��@Z�\@Zn�@Y�^@Y7L@Xr�@X �@W+@V��@VV@Vv�@V��@Vȴ@Vȴ@V@U��@Up�@UV@Tz�@T(�@S�m@Sƨ@St�@R��@R-@Q�^@Qx�@Q�@P��@PĜ@P��@P�u@O�@O��@Ol�@O
=@N��@Nff@N{@M�T@M�-@Mp�@MO�@M?}@M�@MV@L�@L�/@Lz�@K��@J�@Ko@J�H@J�\@J^5@J�@JJ@I��@I�^@I�^@IG�@H��@H�u@HQ�@Hb@G�w@G;d@Fȴ@F�+@Fv�@FV@F5?@F@E@Ep�@Ep�@E?}@D�/@D�D@C�m@Ct�@C"�@B��@BM�@B-@A�^@A�7@A�7@Ax�@A7L@@��@@Ĝ@@bN@?�w@?��@?K�@?
=@>�@>ȴ@>v�@>5?@=�T@=O�@<�j@<z�@<Z@<I�@<(�@;��@;�m@;��@;��@;t�@:J@9�^@9��@8�`@8��@8��@8r�@7�@7l�@7+@6�R@7
=@6��@6ȴ@6��@6�+@6V@6V@6{@5V@4�@4�D@4Z@4�@3o@2��@2��@2�\@2~�@2�\@2��@2~�@2^5@2-@1�#@1�@1��@1��@1��@1�^@1��@1%@0Ĝ@0�9@0�9@0Ĝ@0Ĝ@0Ĝ@0�u@0r�@01'@0b@0b@/�@/�@.�R@.v�@.E�@.{@-�-@-p�@-/@,��@,j@,I�@+��@+�
@+��@*�\@*=q@*�@)��@)�#@)��@)�7@)7L@(��@(��@(b@'�;@'�;@'�w@'�@'��@'|�@'l�@'�@'
=@&�@&��@&$�@&{@&ff@&V@&$�@%�T@%��@%?}@%/@%/@%V@$�j@#��@#ƨ@#33@"��@"n�@"M�@!�@!��@!hs@!��@!��@!��@!��@!��@!x�@!hs@!G�@!7L@!�@ �9@ bN@  �@   @�@�w@�@;d@��@��@��@��@��@E�@$�@@�h@�@p�@O�@�@�/@��@��@��@z�@�@��@�
@�F@��@��@��@�@t�@dZ@S�@33@o@��@�\@n�@��@��@��@��@��@x�@&�@��@�9@�@A�@  @��@|�@\)@�@��@v�@ff@$�@@�-@p�@`B@`B@?}@�@�@V@��@�@�j@Z@9X@�m@t�@S�@33@�H@�!@�\@~�@^5@M�@M�@^5@-@J@�#@�7@x�@x�@X@&�@��@Ĝ@�9@�9@�@b@�;@��@��@;d@+@�@�R@��@��@��@��@�+@v�@ff@$�@@�@@�-@�@V@�@z�@Z@Z@I�@I�@I�@9X@9X@(�@1@��@�F@33@@
��@
��@
n�@
�@
J@
J@
J@	��@	��@	�#@	G�@	7L@	&�@	&�@	�@	�@	%@Ĝ@��@r�@Q�@  @�w@K�@�@�y@�@ȴ@��@��1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 A�r�A�p�A�n�A�n�A�r�A�r�A�p�A�n�A�dZA�$�A�hsA��A�A��A��/A���A���A�C�A���A�\)A��/A�r�A���A��!A�^5A�A��\A��A�z�A���A��A���A���A�ƨA��A�`BA�~�A���A�"�A�p�A�K�A��A���A�;dA�l�A���A��+A�S�A�z�A�ȴA�A��DA��A�~�A�VA���A�I�A��A�=qA��mA�jA��hA���A�A���A��mA��A��yA���A��#A��hA�l�A��#A�(�A��-A�  A�?}A���A�t�A�;dA��A���A��TA��!A�t�A�A��RA�Q�A�E�A���A���A��hA�+A�ĜA�G�A���A��A�S�A���A���A��yA�S�A�JA��mA��PA�;dA���A�&�A�hsA�n�A�dZA��A��yA�z�A�-A�A�A��A��^A�%A�ffA��^A���A�I�A�G�A��`A�(�A���A�%A~1A|�\Az{Ax5?Aw��Au�7As
=Aq�Ap��Ao�AoVAn�9An5?Am��Aln�Ak��Aj�!Ah�yAh �Ag�TAg��Af�Af��Ae�AcO�AbA�AbbAa�AaAax�Aa�A`z�A`5?A_�A^ĜA^�A]�wA\��A[�A[hsAZz�AYdZAW�AV�AUl�AU33AT��AS�ASp�AS�AR��ARQ�AQ�7AP�AO�#AN��AM�AK�AK�AJA�AI�mAIK�AH��AH-AGS�AF5?AE|�AEhsAE33AD�AC�ABr�AA��A@�uA>�A=A;�mA;�hA:�yA9�A9`BA9
=A8�9A89XA7dZA6�uA2��A133A/�;A.��A.I�A*v�A(n�A'A'��A'l�A'?}A&��A&��A&1'A%�PA%
=A$=qA"r�A��A�DAA��A�AE�A�A��AdZA/AA��A�wAG�A��AA�A �AA|�A��AĜAbNA��Al�A��A��A�AjA�A"�A	��A��AffA�-A��A�9A$�A��AoA�Az�AoA ffA E�A -A b@��w@��\@��@�E�@���@�1'@��
@���@�~�@��@�&�@�K�@�E�@�7@��/@�b@@�9X@�^5@�r�@�
=@�bN@��;@㕁@�@�x�@�&�@�;d@ܼj@�
=@�n�@�E�@�V@ى7@�Ĝ@��@Ձ@���@�r�@��m@��@Ь@�l�@�+@���@·+@�$�@̼j@���@ʟ�@ɡ�@�  @���@���@�G�@�j@�@��m@�J@�r�@��P@��j@�E�@��@��-@���@�@�n�@��T@��h@��@�Q�@���@�?}@��@��m@�|�@��y@�@���@��@���@��+@�%@���@�l�@�+@���@�=q@��h@�%@�z�@��m@��P@��@��@�ȴ@���@�n�@�=q@���@���@���@�7L@��/@���@�33@�@���@���@��@��/@�9X@�\)@�@��y@��+@�^5@�M�@�$�@���@�V@�1@�33@��+@�@�p�@��@��@�\)@���@�n�@�E�@�$�@�J@��h@��@�j@�1'@�1@��;@��w@���@��P@�dZ@�;d@��@�
=@��y@�ȴ@��\@�M�@��^@���@�1'@�  @��
@�33@��@�@���@��h@�p�@�7L@���@���@��u@��u@��u@���@��u@��u@��D@��@�j@�Q�@��@��@��
@���@��@�o@��+@�=q@���@�p�@�G�@�G�@�&�@�V@���@��D@��@�Z@�A�@�b@l�@~�y@}@}V@|�/@|I�@|1@{�m@{��@{�@z�@y��@yx�@x�u@xA�@w�;@w|�@wl�@wl�@wl�@w\)@w;d@w;d@w�@v�+@u��@u?}@u�@uV@t��@t�@t�D@t�D@tI�@s��@sdZ@sS�@sS�@s33@r�!@rJ@q��@q�#@q�#@q��@q��@q%@o��@oK�@n�@nE�@m�h@m?}@k�m@k��@kt�@kC�@j��@j�!@j-@ix�@h�`@hbN@hA�@h1'@h �@h �@hb@g�@g�;@g��@gl�@g;d@g
=@fv�@fff@e�@e��@ep�@e/@d��@cƨ@c�@cS�@cC�@co@b^5@a�^@a�7@a7L@`�`@`�9@`��@`�u@`A�@_�@_|�@_K�@_;d@_�@_
=@^��@^�y@^��@^�y@^�y@^�@^��@^��@^�+@^ff@]��@]`B@]/@]V@\��@\��@\�D@\j@\Z@\9X@\(�@\�@[�
@[S�@[o@Z�H@Z��@Z�\@Zn�@Y�^@Y7L@Xr�@X �@W+@V��@VV@Vv�@V��@Vȴ@Vȴ@V@U��@Up�@UV@Tz�@T(�@S�m@Sƨ@St�@R��@R-@Q�^@Qx�@Q�@P��@PĜ@P��@P�u@O�@O��@Ol�@O
=@N��@Nff@N{@M�T@M�-@Mp�@MO�@M?}@M�@MV@L�@L�/@Lz�@K��@J�@Ko@J�H@J�\@J^5@J�@JJ@I��@I�^@I�^@IG�@H��@H�u@HQ�@Hb@G�w@G;d@Fȴ@F�+@Fv�@FV@F5?@F@E@Ep�@Ep�@E?}@D�/@D�D@C�m@Ct�@C"�@B��@BM�@B-@A�^@A�7@A�7@Ax�@A7L@@��@@Ĝ@@bN@?�w@?��@?K�@?
=@>�@>ȴ@>v�@>5?@=�T@=O�@<�j@<z�@<Z@<I�@<(�@;��@;�m@;��@;��@;t�@:J@9�^@9��@8�`@8��@8��@8r�@7�@7l�@7+@6�R@7
=@6��@6ȴ@6��@6�+@6V@6V@6{@5V@4�@4�D@4Z@4�@3o@2��@2��@2�\@2~�@2�\@2��@2~�@2^5@2-@1�#@1�@1��@1��@1��@1�^@1��@1%@0Ĝ@0�9@0�9@0Ĝ@0Ĝ@0Ĝ@0�u@0r�@01'@0b@0b@/�@/�@.�R@.v�@.E�@.{@-�-@-p�@-/@,��@,j@,I�@+��@+�
@+��@*�\@*=q@*�@)��@)�#@)��@)�7@)7L@(��@(��@(b@'�;@'�;@'�w@'�@'��@'|�@'l�@'�@'
=@&�@&��@&$�@&{@&ff@&V@&$�@%�T@%��@%?}@%/@%/@%V@$�j@#��@#ƨ@#33@"��@"n�@"M�@!�@!��@!hs@!��@!��@!��@!��@!��@!x�@!hs@!G�@!7L@!�@ �9@ bN@  �@   @�@�w@�@;d@��@��@��@��@��@E�@$�@@�h@�@p�@O�@�@�/@��@��@��@z�@�@��@�
@�F@��@��@��@�@t�@dZ@S�@33@o@��@�\@n�@��@��@��@��@��@x�@&�@��@�9@�@A�@  @��@|�@\)@�@��@v�@ff@$�@@�-@p�@`B@`B@?}@�@�@V@��@�@�j@Z@9X@�m@t�@S�@33@�H@�!@�\@~�@^5@M�@M�@^5@-@J@�#@�7@x�@x�@X@&�@��@Ĝ@�9@�9@�@b@�;@��@��@;d@+@�@�R@��@��@��@��@�+@v�@ff@$�@@�@@�-@�@V@�@z�@Z@Z@I�@I�@I�@9X@9X@(�@1@��@�F@33@@
��@
��@
n�@
�@
J@
J@
J@	��@	��@	�#@	G�@	7L@	&�@	&�@	�@	�@	%@Ĝ@��@r�@Q�@  @�w@K�@�@�y@�@ȴ@��@��1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 B=qB=qB<jB<jB<jB<jB<jB<jB;dB>wBF�BF�BF�BF�BE�BE�BD�B=qB33B0!B0!B0!B49B6FB8RB7LB49B33B2-B1'B-B.B'�B �B�B�B�B�BuBDB
=B1BB��B��B�B�B�B�`B�TB�ZB�B�B�fB�NB�#B��B��BB�qB�^B�B��B��B�'B�-B�XB��BɺB��BɺBƨB�wB�'B��B��B�DB� Bx�Bq�Bp�Bn�Bm�Bk�BiyBbNB\)BVB@�B7LB2-B�B{BJB  B�B�;B��B�RB�!B��B��B�hB�VB�JB�=B�1B~�Bp�B_;BL�BE�BB�B:^B8RB/B(�B'�B �B�B	7B
��B
�B
�B
�sB
�B
��B
�}B
�!B
��B
�=B
t�B
r�B
`BB
J�B
;dB
6FB
,B
'�B
%�B
"�B
�B
�B
uB
VB
  B	��B	��B	�B	�B	�B	�mB	�B	��B	��B	��B	��B	��B	ȴB	ŢB	B	�wB	�RB	�9B	�'B	�B	��B	��B	��B	��B	�PB	�+B	�B	� B	|�B	y�B	u�B	t�B	r�B	r�B	o�B	m�B	hsB	dZB	]/B	VB	Q�B	L�B	J�B	I�B	F�B	D�B	@�B	<jB	7LB	6FB	5?B	33B	/B	'�B	#�B	�B	�B	\B	%B	B��B��B�B�B�B�fB�;B�
B�wB�3B�B�B��B��B��B�{B�oB�oB�hB�bB�\B�\B�\B�PB�DB�B|�Bx�Bv�Bs�Br�Bq�Bp�Bn�Bm�Bl�BiyBbNB_;B]/B\)B[#BZBYBXBVBQ�BP�BN�BM�BM�BK�BK�BJ�BK�BC�BB�B?}B<jB=qB9XB8RB6FB7LB7LB6FB7LB6FB49B49B49B33B33B2-B0!B/B-B-B,B+B)�B)�B(�B)�B'�B'�B&�B%�B&�B$�B$�B$�B$�B#�B"�B"�B#�B!�B �B&�B!�B"�B"�B"�B"�B"�B"�B%�B$�B$�B$�B$�B$�B)�B'�B'�B'�B'�B'�B(�B)�B)�B,B-B.B/B/B0!B33B5?B7LB8RB9XB>wB@�BA�BA�BC�BF�BG�BH�BI�BJ�BK�BO�BQ�BR�BVBVBW
BYB\)B]/B^5BaHBffBk�Bk�Bl�Bn�Bo�Bq�Bs�Bu�Bw�Bx�By�Bz�Bz�B{�B{�B|�B}�B~�B~�B�B�B�B�+B�1B�7B�=B�\B�uB��B��B��B��B��B��B��B��B��B��B�!B�9B�LB�^B�dB��BŢBǮB��B��B��B��B��B��B�B�#B�)B�/B�;B�;B�BB�HB�NB�TB�ZB�ZB�`B�fB�mB�sB�B�B��B��B��B��B	B	B	B	B	B	%B	1B		7B		7B		7B		7B		7B		7B		7B		7B	
=B	
=B	DB	JB	PB	VB	\B	bB	{B	�B	�B	�B	!�B	#�B	"�B	#�B	#�B	'�B	+B	+B	.B	/B	2-B	6FB	7LB	:^B	<jB	>wB	@�B	B�B	B�B	C�B	D�B	E�B	H�B	J�B	N�B	P�B	R�B	VB	W
B	W
B	W
B	W
B	YB	XB	ZB	^5B	bNB	cTB	cTB	cTB	e`B	e`B	e`B	e`B	ffB	hsB	hsB	hsB	hsB	hsB	jB	m�B	n�B	o�B	o�B	o�B	p�B	s�B	x�B	{�B	}�B	~�B	� B	�B	�B	�%B	�%B	�%B	�+B	�+B	�1B	�=B	�JB	�\B	�bB	�bB	�bB	�bB	�hB	�hB	�hB	�oB	�{B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�'B	�-B	�9B	�FB	�LB	�LB	�LB	�RB	�XB	�jB	�jB	�qB	�qB	�wB	�wB	�wB	�wB	�wB	�wB	�wB	�}B	�}B	��B	��B	B	ĜB	ĜB	ŢB	ŢB	ƨB	ǮB	ǮB	ǮB	ȴB	ȴB	ȴB	ɺB	��B	��B	��B	��B	��B	��B	��B	�
B	�B	�#B	�5B	�BB	�NB	�ZB	�`B	�mB	�mB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
  B
B
%B
1B
	7B

=B
DB
DB
DB
DB
DB
DB
JB
JB
JB
PB
PB
VB
\B
bB
bB
bB
bB
hB
hB
oB
oB
uB
{B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
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
!�B
!�B
!�B
!�B
!�B
!�B
!�B
!�B
!�B
"�B
"�B
"�B
#�B
#�B
#�B
$�B
$�B
%�B
%�B
&�B
'�B
(�B
)�B
)�B
)�B
)�B
)�B
)�B
,B
-B
-B
.B
.B
0!B
0!B
0!B
2-B
33B
33B
49B
49B
5?B
5?B
5?B
6FB
5?B
5?B
5?B
5?B
5?B
5?B
5?B
6FB
7LB
8RB
8RB
9XB
9XB
9XB
:^B
:^B
:^B
;dB
;dB
<jB
<jB
<jB
<jB
<jB
=qB
=qB
=qB
=qB
=qB
=qB
=qB
>wB
>wB
>wB
>wB
>wB
>wB
?}B
?}B
?}B
?}B
@�B
B�B
B�B
B�B
C�B
C�B
C�B
C�B
C�B
D�B
D�B
D�B
D�B
E�B
E�B
F�B
F�B
G�B
H�B
H�B
I�B
I�B
I�B
I�B
I�B
I�B
I�B
I�B
H�B
H�B
I�B
I�B
J�B
J�B
L�B
L�B
L�B
L�B
M�B
M�B
M�B
M�B
M�B
M�B
M�B
M�B
L�B
M�B
M�B
N�B
N�B
N�B
N�B
N�B
N�B
N�B
P�B
P�B
P�B
P�B
P�B
P�B
P�B
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
T�B
T�B
T�B
T�B
T�B
T�B
T�B
T�B
T�B
T�B
VB
VB
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
XB
YB
YB
YB
ZB
ZB
ZB
[#B
[#B
[#B
\)B
\)B
\)B
\)B
\)B
]/B
]/B
]/B
]/B
^5B
^5B
^5B
^5B
^5B
^5B
^5B
_;B
_;B
_;B
`BB
`BB
`BB
`BB
`BB
`BB
`BB
aHB
aHB
aHB
aHB
aHB
aHB
aHB
bNB
bNB
bNB
bNB
bNB
cTB
cTB
cTB
cTB
cTB
dZB
dZB
dZB
dZB
e`B
e`B
e`B
e`B
e`B
e`B
e`B
e`B
e`B
e`B
e`B
ffB
ffB
ffB
ffB
ffB
ffB
gmB
hsB
hsB
hsB
hsB
hsB
hsB
hsB
hsB
hsB
hsB
hsB
hsB
iyB
iyB
iyB
jB
jB
jB
jB
jB
k�B
k�B
k�B
k�B
k�B
k�B
l�B
l�B
l�B
l�B
l�B
l�B
l�B
l�B
m�B
m�B
m�B
n�B
n�B
n�B
o�B
o�B
o�B
o�B
o�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 B=qB=qB<jB<jB<jB<�B<�B<�B<6B?�BG+BF�BF�BF�BE�BF%BE�B>�B4B1AB1'B1'B4�B7B9$B8B4�B3�B3�B3B/�B0�B)�B"�B;BxB�BYB�B�B
�B	RBMB �B�8B�!B�wB�]B�B��B�zB�B��B�mB�TB�B��B�BBÖB��B��B�B��B�B�AB�-B�rB��B�#B�}B�rB��B� B�|B�yB�#B�JB��ByXBrBp�Bn�BnBl=BjeBc:B]~BW�BA�B8RB4�B�B�B�B�B�'B�bB�hB��B��B�B�KB��B�BB�6B�xB��B��Br�Ba-BM�BFYBC�B;B:B0B)�B)�B"NB?BB
��B
� B
�B
�B
ڠB
ΊB
��B
�GB
�|B
�JB
vB
uZB
b�B
L~B
<jB
7�B
,�B
(�B
&�B
#�B
!B
�B
�B
.B
 �B	�RB	�?B	�B	�OB	��B	��B	�1B	�4B	�B	�"B	�6B	�^B	�lB	�%B	�{B	�cB	�$B	��B	�aB	�B	��B	� B	�/B	�_B	�BB	��B	��B	��B	}�B	zxB	vFB	u%B	s�B	s�B	p�B	n�B	j0B	f2B	^�B	W$B	R�B	M�B	K�B	J�B	G�B	E�B	A�B	=<B	7�B	6�B	6B	5B	0�B	)DB	%zB	 �B	�B	�B	�B	B�B��B�3B�5B�qB��B�HB��B��B�B��B��B�B��B�_B��B��B��B��B� B�B�HB�bB��B��B��B~�By�Bw�BtTBsMBr-Bq'Bo5Bn�BoiBlBc�B_�B]�B\�B[qBZ�BZQBZ7BW$BR�BQ�BOvBNpBNVBL0BL~BLdBN�BE9BC�B@OB=�B?�B:�B9>B6�B88B7�B7LB8�B7B4�B4�B4�B3�B49B3�B1�B/�B-�B-wB,�B+kB*B*�B*0B*�B(�B(�B'�B'8B(�B&LB&2B&B&fB$tB#TB#�B$ZB"�B"hB(sB"�B#:B#B#:B#�B#�B$tB&LB%`B%`B%zB%�B&fB*�B(>B(XB(>B(sB(�B)�B*�B*�B-)B-�B.�B/�B0;B1�B4�B6�B8�B9rB;JB?�BABA�BBuBD�BG+BHBIBJXBKxBMBP�BRoBS�BV�BV�BW�BY�B\�B]�B_!BbhBg8Bk�Bk�BmBo Bp!Br-Bt9Bv+BxBy$BzB{B{B|B|B}<B~(BHBcB�oB��B��B�_B��B��B�B��B��B�EB��B��B�B�B�B�&B�`B��B��B��B��B��B��B�B� B��B�B�B�B�B�B�HB�[B�mB�WB�xB�dB�VB�VB�\B�|B�B�nB�tB�B�B�B�B��B�)B�-B��B�+B�fB��B	-B	MB	3B	MB	SB	tB	fB		RB		RB		RB		7B		RB		lB		RB		RB	
XB	
rB	xB	~B	�B	�B	�B	�B	�B	�B	�B	B	!�B	#�B	#B	$B	$&B	(
B	+6B	+6B	./B	/OB	2|B	6�B	7�B	:�B	<�B	>�B	@�B	B�B	B�B	C�B	EB	FB	H�B	KB	OB	QB	S&B	VB	W
B	W?B	W$B	W$B	Y1B	XEB	ZkB	^�B	bhB	cnB	cnB	cnB	ezB	ezB	ezB	e�B	f�B	h�B	h�B	h�B	h�B	h�B	j�B	m�B	n�B	o�B	o�B	o�B	p�B	t9B	y	B	|B	~(B	HB	�4B	�uB	�9B	�?B	�?B	�tB	�EB	�_B	��B	�rB	�~B	�vB	�bB	�}B	�bB	�}B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	� B	�B	�
B	�B	�DB	�KB	�CB	�AB	�GB	�TB	�`B	�fB	��B	��B	��B	��B	��B	��B	��B	��B	�wB	�wB	�wB	��B	�wB	��B	��B	��B	��B	��B	��B	��B	ĶB	ĶB	żB	żB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�:B	�2B	�YB	�KB	�qB	�jB	�\B	�NB	�ZB	�`B	�B	�B	�B	�B	�B	��B	�B	�B	��B	��B	��B	��B	��B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�"B	�BB
 4B
B
?B
KB
	RB

XB
DB
^B
^B
^B
xB
xB
dB
dB
~B
�B
�B
�B
vB
bB
}B
�B
}B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
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
!�B
!�B
!�B
!�B
!�B
!�B
!�B
!�B
"4B
"�B
"�B
# B
#�B
#�B
#�B
%B
%B
&B
%�B
&�B
($B
)B
*B
*0B
*B
*B
*0B
*KB
,"B
-)B
-CB
.IB
.cB
0;B
0;B
0;B
2GB
33B
33B
4TB
4nB
5tB
5tB
5?B
6FB
5?B
5?B
5tB
5ZB
5tB
5ZB
5?B
6`B
7LB
8RB
8lB
9rB
9rB
9rB
:�B
:xB
:�B
;�B
;�B
<�B
<�B
<�B
<�B
<�B
=�B
=�B
=�B
=�B
=�B
=�B
=�B
>�B
>�B
>�B
>�B
>�B
>�B
?�B
?�B
?�B
?�B
@�B
B�B
B�B
B�B
C�B
C�B
C�B
C�B
C�B
D�B
D�B
D�B
D�B
E�B
E�B
F�B
F�B
G�B
H�B
H�B
I�B
I�B
I�B
J	B
I�B
I�B
I�B
I�B
H�B
H�B
I�B
I�B
J�B
J�B
L�B
L�B
L�B
L�B
M�B
M�B
M�B
M�B
M�B
M�B
M�B
NB
MB
M�B
M�B
N�B
OB
N�B
N�B
N�B
N�B
OB
Q B
Q B
Q B
Q B
P�B
Q B
Q B
RB
SB
R�B
R�B
SB
TB
TB
TB
TB
T�B
T�B
UB
T�B
UB
UB
T�B
T�B
UB
U2B
UB
VB
VB
W$B
W$B
W$B
W
B
XB
XEB
X+B
X+B
YB
Y1B
Y1B
ZQB
Z7B
Z7B
[WB
[=B
[qB
\CB
\CB
\CB
\]B
\CB
]IB
]/B
]IB
]IB
^OB
^5B
^5B
^5B
^OB
^OB
^jB
_pB
_VB
_pB
`\B
`\B
`vB
`\B
`\B
`BB
`\B
aHB
aHB
abB
abB
abB
abB
abB
bhB
bNB
bhB
bhB
bhB
c:B
cTB
cTB
cnB
c�B
d�B
d�B
dtB
dtB
ezB
ezB
ezB
e`B
e`B
e`B
e`B
e`B
e`B
ezB
ezB
f�B
f�B
f�B
f�B
f�B
f�B
g�B
h�B
h�B
hsB
hsB
hsB
h�B
h�B
hsB
hsB
h�B
h�B
h�B
i�B
i�B
i�B
j�B
j�B
j�B
j�B
jB
k�B
k�B
k�B
k�B
k�B
k�B
l�B
l�B
l�B
l�B
l�B
l�B
l�B
l�B
m�B
m�B
m�B
n�B
n�B
n�B
o�B
o�B
o�B
o�B
o�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 @��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
PRES            TEMP            PSAL            PRES            TEMP            PSAL            PRES            TEMP            PSAL            PRES_ADJ=PRES-SP,  where SP is SURFACE PRESSURE from next cycle; PRES_ADJ_ERR=Manufacturer sensor accuracy                                                                                                                                                      TEMP_ADJ=TEMP; TEMP_ADJ_ERR=Manufacturer sensor accuracy                                                                                                                                                                                                        PSAL_ADJ = RecalS= psal(PRES_ADJ,TEMP,Conductivity)                                                                                                                                                                                                             None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            PSAL_ADJ = celltm_sbe41(RecalS,TEMP,P,elapsed_time,alpha,tau); elapsed_time=P/mean_rise_rate; P=dbar since the start of the profile for each samples                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            PSAL_ADJ_ERR=max(RecalS & CTM error, 0.01(PSS-78))                                                                                                                                                                                                              SP=0.02(dbar)                                                                                                                                                                                                                                                   None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            alpha=0.0267C, tau=18.6s, mean_rise_rate = 0.09 dbar/second                                                                                                                                                                                                     None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            Pressure Correction using reported SURFACE PRESSURE                                                                                                                                                                                                             None                                                                                                                                                                                                                                                            Salinity Recalculation using PRES_ADJ                                                                                                                                                                                                                           None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            Correction for conductivity cell thermal mass error(CTM), Johnson et al., 2007, JAOT                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            No adjustment is needed                                                                                                                                                                                                                                         201606080922242016060809222420160608092224201806221138352018062211383520180622113835201804050401032018040504010320180405040103  JA  ARFMdecpA19c                                                                20160523153514  IP                  G�O�G�O�G�O�                JA  ARFMfmtp2.8                                                                 20160523063529  IP                  G�O�G�O�G�O�                JA  ARCAsspa3.1b                                                                20160523063530  IP  PRES            G�O�G�O�G�O�                JA  ARGQrqcppo_c                                                                20160523063530  QCP$                G�O�G�O�G�O�              3CJA  ARGQrqcpt19d                                                                20160523063531  QCP$                G�O�G�O�G�O�           80000JA  ARGQaqcpt19d                                                                20160523063531  QCP$                G�O�G�O�G�O�           80000JA  ARGQrqcp2.8e                                                                20160523063531  QCP$                G�O�G�O�G�O�            FB40JA  ARGQaqcp2.8e                                                                20160523063531  QCP$                G�O�G�O�G�O�            FB40JA      jafc1.0                                                                 20160523063531                      G�O�G�O�G�O�                JA  ARUP                                                                        20160523102044                      G�O�G�O�G�O�                JM  ARGQJMQC2.0                                                                 20160530023542  CV  JULD            G�O�G�O�F�iq                JM  ARCAJMQC2.0                                                                 20160608002224  CV  PRES_ADJUSTED   G�O�G�O�G�O�                JM  ARCAJMQC2.0                                                                 20160608002224  CV  PSAL_ADJUSTED   G�O�G�O�G�O�                JM  ARSQOW  1.1 2017V1                                                          20180404190103  IP  PSAL_ADJUSTED   G�O�G�O�G�O�                JM  ARCAJMTM1.0                                                                 20180622023835  CV  PSAL_ADJUSTED   G�O�G�O�G�O�                JA  ARDU                                                                        20200115101516                      G�O�G�O�G�O�                