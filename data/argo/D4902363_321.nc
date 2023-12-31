CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       JMA    source        
Argo float     history       b2019-01-15T00:37:49Z creation;2019-01-15T00:37:55Z conversion to V3.1;2019-12-19T07:23:20Z update;     
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
resolution        =���   axis      Z        P  9�   PRES_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  H�   PRES_ADJUSTED            
      	   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     P  L�   PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  \   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     
_FillValue        G�O�   	valid_min         �      	valid_max         B      C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     P  _�   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  o4   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     
_FillValue        G�O�   	valid_min         �      	valid_max         B      C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     P  s   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �X   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    
_FillValue        G�O�   	valid_min         @      	valid_max         B$     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     P  �,   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �|   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    
_FillValue        G�O�   	valid_min         @      	valid_max         B$     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     P  �P   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ��   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         decibar    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     P  �t   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         degree_Celsius     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     P  ��   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     P  �   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
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
_FillValue                    ��Argo profile    3.1 1.2 19500101000000  20190115003749  20200115131516  4902363                                                                 JAMSTEC                                                         PRES            TEMP            PSAL              AA   JA  I2_0576_321                     2C  D   NAVIS_A                         0576                            ARGO 092515                     863 @؟���#�1   @؟�����@9����dNV�u1   GPS     A   B   B   Primary sampling: averaged [bin average for 1 Hz CTD]                                                                                                                                                                                                              @�  @�  A   A   A@  A`  A�  A�  A�  A�  A�  A�  A�  A�  B   B  B  B  B   B(  B0  B8  B@  BH  BP  BX  B`  Bh  Bp  Bx  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  C   C  C  C  C  C
  C  C  C  C  C  C  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dt� Du  Du� Dv  Dv� Dw  Dw� Dx  Dx� Dy  Dy� Dz  Dz� D{  D{� D|  D|� D}  D}� D~  D~� D  D� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D��3D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D��3D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�3D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D D�� D�  D�@ DÀ D�� D�  D�@ DĀ D�� D�  D�@ Dŀ D�� D�  D�@ Dƀ D�� D�  D�@ Dǀ D�� D�  D�@ DȀ D�� D�  D�@ Dɀ D�� D�  D�@ Dʀ D�� D�  D�@ Dˀ D�� D�  D�@ D̀ D�� D�  D�@ D̀ D�� D�  D�@ D΀ D�� D�  D�@ Dπ D�� D�  D�@ DЀ D�� D�  D�@ Dр D�� D�  D�@ DҀ D�� D�  D�@ DӀ D�� D�  D�@ DԀ D�� D�  D�C3DՃ3D�� D�  D�@ Dր D�� D�  D�@ D׀ D�� D�  D�@ D؀ D�� D�  D�@ Dـ D�� D�  D�@ Dڀ D�� D�  D�<�Dۀ D�� D�  D�@ D܀ D�� D�  D�@ D݀ D�� D�  D�@ Dހ D�� D�  D�@ D߀ D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�3D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�3D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�P 11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111@~�R@�\)@�\)A�A?�A_�A�A��
A��
A��
A��
A��
A��
A��
A��
B�B�B�B�B'�B/�B7�B?�BG�BO�BW�B_�Bg�Bo�Bw�B�B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���C��C��C��C��C	��C��C��C��C��C��C��C��C��C��C��C��C!��C#��C%��C'��C)��C+��C-��C/��C1��C3��C5��C7��C9��C;��C=��C?��CA��CC��CE��CG��CI��CK��CM��CO��CQ��CS��CU��CW��CY��C[��C]��C_��Ca��Cc��Ce��Cg��Ci��Ck��Cm��Co��Cq��Cs��Cu��Cw��Cy��C{��C}��C��C��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��C��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qD ~�D ��D~�D��D~�D��D~�D��D~�D��D~�D��D~�D��D~�D��D~�D��D	~�D	��D
~�D
��D~�D��D~�D��D~�D��D~�D��D~�D��D~�D��D~�D��D~�D��D~�D��D~�D��D~�D��D~�D��D~�D��D~�D��D~�D��D~�D��D~�D��D~�D��D~�D��D~�D��D~�D��D ~�D ��D!~�D!��D"~�D"��D#~�D#��D$~�D$��D%~�D%��D&~�D&��D'~�D'��D(~�D(��D)~�D)��D*~�D*��D+~�D+��D,~�D,��D-~�D-��D.~�D.��D/~�D/��D0~�D0��D1~�D1��D2~�D2��D3~�D3��D4~�D4��D5~�D5��D6~�D6��D7~�D7��D8~�D8��D9~�D9��D:~�D:��D;~�D;��D<~�D<��D=~�D=��D>~�D>��D?~�D?��D@~�D@��DA~�DA��DB~�DB��DC~�DC��DD~�DD��DE~�DE��DF~�DF��DG~�DG��DH~�DH��DI~�DI��DJ~�DJ��DK~�DK��DL~�DL��DM~�DM��DN~�DN��DO~�DO��DP~�DP��DQ~�DQ��DR~�DR��DS~�DS��DT~�DT��DU~�DU��DV~�DV��DW~�DW��DX~�DX��DY~�DY��DZ~�DZ��D[~�D[��D\~�D\��D]~�D]��D^~�D^��D_~�D_��D`~�D`��Da~�Da��Db~�Db��Dc~�Dc��Dd~�Dd��De~�De��Df~�Df��Dg~�Dg��Dh~�Dh��Di~�Di��Dj~�Dj��Dk~�Dk��Dl~�Dl��Dm~�Dm��Dn~�Dn��Do~�Do��Dp~�Dp��Dq~�Dq��Dr~�Dr��Ds~�Ds��Dt~�Dt��Du~�Du��Dv~�Dv��Dw~�Dw��Dx~�Dx��Dy~�Dy��Dz~�Dz��D{~�D{��D|~�D|��D}~�D}��D~~�D~��D~�D��D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D���D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D���D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D¿\D��\D�?\D�\Dÿ\D��\D�?\D�\DĿ\D��\D�?\D�\Dſ\D��\D�?\D�\Dƿ\D��\D�?\D�\Dǿ\D��\D�?\D�\Dȿ\D��\D�?\D�\Dɿ\D��\D�?\D�\Dʿ\D��\D�?\D�\D˿\D��\D�?\D�\D̿\D��\D�?\D�\DͿ\D��\D�?\D�\Dο\D��\D�?\D�\DϿ\D��\D�?\D�\Dп\D��\D�?\D�\Dѿ\D��\D�?\D�\Dҿ\D��\D�?\D�\Dӿ\D��\D�?\D�\DԿ\D��\D�B�DՂ�Dտ\D��\D�?\D�\Dֿ\D��\D�?\D�\D׿\D��\D�?\D�\Dؿ\D��\D�?\D�\Dٿ\D��\D�?\D�\Dڿ\D��\D�<)D�\Dۿ\D��\D�?\D�\Dܿ\D��\D�?\D�\Dݿ\D��\D�?\D�\D޿\D��\D�?\D�\D߿\D��\D�?\D�\D�\D��\D�?\D�\D�\D��\D�?\D�\D�\D��\D�?\D�\D�\D��\D�?\D�\D�\D��\D�?\D�\D�\D��\D�?\D�\D�\D��\D�?\D�\D�\D��D�?\D�\D�\D��\D�?\D�\D�\D��\D�?\D�\D�\D��\D�?\D낏D�\D��\D�?\D�\D�\D��\D�?\D�\D��\D��\D�?\D�\D�\D��\D�?\D�\D�\D��\D�?\D�\D�\D��\D�?\D�\D�\D��\D�?\D�\D�\D��\D�?\D�\D�\D��\D�?\D�\D��\D��\D�O\11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111A��PA��PA��PA��\A��hA��hA��\A��hA��hA��hA��hA��hA��hA��hA��uA���A���A���A���A��hA��hA��\A��7A��A�|�A�|�A�z�A�x�A�x�A�v�A�v�A�t�A�p�A�jA�^5A�A�A�{A��A�ȴA���A���A��PA��+A��A�p�A�jA�ZA�=qA�&�A�{A���A��wA�VA��A�"�A��!A���A�;dA���A�I�A��!A�  A�E�A�XA��A��FA�^5A�ĜA�dZA�O�A�{A���A��A���A��uA��PA�-A��A���A���A���A�l�A�I�A��
A�ĜA���A��A���A��\A�Q�A��A�jA�+A�r�A���A��/A��TA�-A�z�A�XA�jA�v�A�XA�XA���A���A��FA��A��PA��A}�Ax�Aw?}Au�As��Aq�wAo��AnffAlv�Ak/Ai��Ai�Ah�+Ag�wAf�AeC�Ac�TAcAbjA`A�A^�A^��A]�;A]
=A\M�A\�A\  A[A[hsA[;dAZ�AX��AW�AWx�AV��AT��AR��AQ�^APz�AO��AN�9AM?}AK��AK`BAJ�AI�7AG\)AGAF�AF^5AE�AD�jAD$�AC�;AC��AC;dAB�ABv�AA�mAAS�A@ȴA@�A@^5A@(�A?�#A?7LA=�TA<(�A;�PA:��A9�TA9�A8�jA8n�A7�hA7VA6��A6bNA6bA5��A4�uA3�A3dZA2��A1XA0~�A/�hA.VA-;dA+�A)�#A)&�A(�DA'��A'|�A'VA%��A$��A#��A#\)A"1'A!�-A!33A �DA�Av�AdZA�
A��A��A  A�A�A�TA�^A"�A�yA��AI�A�A�^A�hA�A��Ar�AA�A��A�HA�A�A7LA�9AbAz�A�A;dA�A��AAbA33A �\A V@���@���@��T@��@�Ĝ@�j@�E�@���@�C�@�+@��T@�%@�v�@�
=@�?}@�9@��@�dZ@���@�-@�%@��@�7@���@�Z@ߕ�@�v�@��@�l�@�"�@�~�@�9X@�^5@ԃ@�=q@љ�@��@�ƨ@��@��@̼j@˅@��@�ff@�x�@��`@ț�@��;@�E�@� �@å�@�V@��7@�V@�b@���@���@�E�@�J@�hs@��u@�1'@�b@�+@�5?@���@��@���@�  @���@�l�@�S�@�C�@�K�@��@���@� �@��@�E�@���@�V@��D@�Z@�(�@��@�dZ@���@�%@��@�r�@�I�@� �@��w@�dZ@�@��H@���@�ȴ@��!@��+@�@��@�z�@�bN@�Z@�Z@� �@�ƨ@��@�?}@�1'@��;@��@�dZ@�dZ@�33@���@�A�@�7L@�1'@��F@�C�@���@���@���@���@�^5@�5?@�%@�"�@�Z@�K�@�;d@�o@��@��\@��@��@��u@�r�@�9X@�ƨ@�33@�ȴ@�^5@��@���@���@�p�@�/@��/@��@��`@��u@�j@�Z@�bN@�I�@� �@�b@� �@�1'@�(�@�b@��
@���@��P@�t�@�C�@�ȴ@�7L@��u@�1'@��@�1@�  @���@��
@��
@���@�ƨ@��@�|�@�\)@�dZ@�dZ@�dZ@�dZ@�\)@�33@�33@�;d@�
=@�V@�M�@�$�@��@��^@�@��-@���@�O�@�&�@���@��u@���@�+@�v�@��@��-@�G�@�V@���@���@�z�@�I�@�1'@�(�@�@~V@}�@|(�@{ƨ@{@y�^@y��@yx�@y&�@x�u@xbN@xQ�@x1'@xb@w��@w��@w+@v�@v�R@v{@u�-@t�/@t1@s@rM�@q�@q&�@p  @o�P@o�@n�R@nV@n5?@m�T@m��@mp�@m?}@l�j@lI�@l9X@l1@k�
@kS�@kC�@k33@j�@j�!@i�#@iX@h�9@g��@g�w@g��@gK�@f�y@f$�@e�-@e�@e�@e�@ep�@e/@d�D@dj@d9X@c�m@c�F@c��@ct�@cC�@c"�@b��@b=q@a��@a�#@a��@a��@ahs@aX@aG�@`��@`A�@_�;@_��@_l�@_K�@^ȴ@^V@^$�@^@]�T@]�@\��@\9X@[��@[dZ@Z�H@ZM�@Y�@Y�@X�@Xb@W�@WK�@W�@V�y@V�R@V��@V��@V5?@U@U`B@T��@T��@T�j@T��@T�D@T�D@Tj@Tj@TZ@TI�@T�@Sƨ@St�@S33@R�@R��@R��@R~�@RM�@R-@Q�^@Q&�@P�`@PQ�@P  @O��@O�@O��@O�@N�R@N�+@N�+@Nv�@NE�@M@MV@K��@K��@Kt�@KdZ@KC�@Ko@J��@J��@J��@J~�@J=q@I��@I�^@I��@I�7@IG�@I7L@I%@H��@H�@H  @G��@G��@G�@G��@G�P@Gl�@Gl�@G+@F��@E�T@E��@Ep�@E/@E/@E/@D�@Dj@D�@C��@B�@B��@B��@B^5@Ahs@@��@@�@@1'@@ �@@ �@@b@@b@?�;@?K�@>�@>V@>{@=@=�h@=�@=`B@=?}@=V@<��@<�@<z�@<�@;��@;C�@;33@;"�@;@:�@:��@:�!@:�\@:^5@:J@9��@9�7@9�7@9x�@9%@8��@8Ĝ@8�9@8�@7��@7l�@7\)@7;d@6�y@6�+@6ff@6V@6$�@6$�@6@5�T@5@5��@5�@4��@4�@3ƨ@3S�@2�@2��@2��@2�\@2^5@1�@17L@1%@1%@0��@0��@0��@0�9@0��@0�u@0�@0bN@0bN@0r�@0bN@0 �@/�@/\)@/+@/
=@.�y@.ȴ@.�R@.��@.v�@.V@-�T@-�-@-`B@-/@,��@,�@,I�@+�
@+�@+dZ@+dZ@+dZ@+S�@+C�@+33@*��@*�@)��@)hs@)%@(�9@(1'@(  @'�;@'��@'K�@&��@&ȴ@&��@&E�@%��@%p�@%O�@%?}@%/@%�@$��@$j@$1@#�m@#C�@"�!@"n�@"=q@!�@!�^@!��@!�7@!hs@ ��@ �9@ �@�;@�w@�w@�P@l�@l�@K�@+@�@�+@V@$�@�T@�-@��@/@9X@ƨ@��@t�@S�@C�@"�@�!@=q@�@��@x�@X@�@Ĝ@�@�@Q�@b@�w@;d@
=@ȴ@v�@E�@@��@��@`B@?}@��@�@��@��@Z@1@�
@�F@S�@�@�\@n�@^5@-@�@��@��@hs@7L@�@�`@�9@�@r�@bN@A�@ �@ �@�@��@l�@K�@
=@��@�R@��@�R@�R@��@�+@V@E�@5?@{@�T@@��@�h@�@`B@O�@�@�@�@Z@9X@�@�
@�F@��@�@t�@S�@"�@
�@
��@
��@
��@
��@
�\@
~�@
^5@
-@
�@	�@	��@	x�@	hs@	7L@�`@Ĝ@�u@r�@bN@A�@ �@��@�w@�@�P@\)@�@�y@ȴ@�R@v�@5?@{@��@@��@�@p�@p�@p�@O�@�@V@�@�/@��@��@�j@�@Z@I�@(�@�
@�F@�@S�@C�@o@�@�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111444111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111A��PA��PA��PA��\A��hA��hA��\A��hA��hA��hA��hA��hA��hA��hA��uA���A���A���A���A��hA��hA��\A��7A��A�|�A�|�A�z�A�x�A�x�A�v�A�v�A�t�A�p�A�jA�^5A�A�A�{A��A�ȴA���A���A��PA��+A��A�p�A�jA�ZA�=qA�&�A�{A���A��wA�VA��A�"�A��!A���A�;dA���A�I�A��!A�  A�E�A�XA��A��FA�^5A�ĜA�dZA�O�A�{A���A��A���A��uA��PA�-A��A���A���A���A�l�A�I�A��
A�ĜA���A��A���A��\A�Q�A��G�O�G�O�A�r�A���A��/A��TA�-A�z�A�XA�jA�v�A�XA�XA���G�O�G�O�A��A��PA��A}�Ax�Aw?}Au�As��Aq�wAo��AnffAlv�Ak/Ai��Ai�Ah�+Ag�wAf�AeC�Ac�TAcAbjA`A�A^�A^��A]�;A]
=A\M�A\�A\  A[A[hsA[;dAZ�AX��AW�AWx�AV��AT��AR��AQ�^APz�AO��AN�9AM?}AK��AK`BAJ�AI�7AG\)AGAF�AF^5AE�AD�jAD$�AC�;AC��AC;dAB�ABv�AA�mAAS�A@ȴA@�A@^5A@(�A?�#A?7LA=�TA<(�A;�PA:��A9�TA9�A8�jA8n�A7�hA7VA6��A6bNA6bA5��A4�uA3�A3dZA2��A1XA0~�A/�hA.VA-;dA+�A)�#A)&�A(�DA'��A'|�A'VA%��A$��A#��A#\)A"1'A!�-A!33A �DA�Av�AdZA�
A��A��A  A�A�A�TA�^A"�A�yA��AI�A�A�^A�hA�A��Ar�AA�A��A�HA�A�A7LA�9AbAz�A�A;dA�A��AAbA33A �\A V@���@���@��T@��@�Ĝ@�j@�E�@���@�C�@�+@��T@�%@�v�@�
=@�?}@�9@��@�dZ@���@�-@�%@��@�7@���@�Z@ߕ�@�v�@��@�l�@�"�@�~�@�9X@�^5@ԃ@�=q@љ�@��@�ƨ@��@��@̼j@˅@��@�ff@�x�@��`@ț�@��;@�E�@� �@å�@�V@��7@�V@�b@���@���@�E�@�J@�hs@��u@�1'@�b@�+@�5?@���@��@���@�  @���@�l�@�S�@�C�@�K�@��@���@� �@��@�E�@���@�V@��D@�Z@�(�@��@�dZ@���@�%@��@�r�@�I�@� �@��w@�dZ@�@��H@���@�ȴ@��!@��+@�@��@�z�@�bN@�Z@�Z@� �@�ƨ@��@�?}@�1'@��;@��@�dZ@�dZ@�33@���@�A�@�7L@�1'@��F@�C�@���@���@���@���@�^5@�5?@�%@�"�@�Z@�K�@�;d@�o@��@��\@��@��@��u@�r�@�9X@�ƨ@�33@�ȴ@�^5@��@���@���@�p�@�/@��/@��@��`@��u@�j@�Z@�bN@�I�@� �@�b@� �@�1'@�(�@�b@��
@���@��P@�t�@�C�@�ȴ@�7L@��u@�1'@��@�1@�  @���@��
@��
@���@�ƨ@��@�|�@�\)@�dZ@�dZ@�dZ@�dZ@�\)@�33@�33@�;d@�
=@�V@�M�@�$�@��@��^@�@��-@���@�O�@�&�@���@��u@���@�+@�v�@��@��-@�G�@�V@���@���@�z�@�I�@�1'@�(�@�@~V@}�@|(�@{ƨ@{@y�^@y��@yx�@y&�@x�u@xbN@xQ�@x1'@xb@w��@w��@w+@v�@v�R@v{@u�-@t�/@t1@s@rM�@q�@q&�@p  @o�P@o�@n�R@nV@n5?@m�T@m��@mp�@m?}@l�j@lI�@l9X@l1@k�
@kS�@kC�@k33@j�@j�!@i�#@iX@h�9@g��@g�w@g��@gK�@f�y@f$�@e�-@e�@e�@e�@ep�@e/@d�D@dj@d9X@c�m@c�F@c��@ct�@cC�@c"�@b��@b=q@a��@a�#@a��@a��@ahs@aX@aG�@`��@`A�@_�;@_��@_l�@_K�@^ȴ@^V@^$�@^@]�T@]�@\��@\9X@[��@[dZ@Z�H@ZM�@Y�@Y�@X�@Xb@W�@WK�@W�@V�y@V�R@V��@V��@V5?@U@U`B@T��@T��@T�j@T��@T�D@T�D@Tj@Tj@TZ@TI�@T�@Sƨ@St�@S33@R�@R��@R��@R~�@RM�@R-@Q�^@Q&�@P�`@PQ�@P  @O��@O�@O��@O�@N�R@N�+@N�+@Nv�@NE�@M@MV@K��@K��@Kt�@KdZ@KC�@Ko@J��@J��@J��@J~�@J=q@I��@I�^@I��@I�7@IG�@I7L@I%@H��@H�@H  @G��@G��@G�@G��@G�P@Gl�@Gl�@G+@F��@E�T@E��@Ep�@E/@E/@E/@D�@Dj@D�@C��@B�@B��@B��@B^5@Ahs@@��@@�@@1'@@ �@@ �@@b@@b@?�;@?K�@>�@>V@>{@=@=�h@=�@=`B@=?}@=V@<��@<�@<z�@<�@;��@;C�@;33@;"�@;@:�@:��@:�!@:�\@:^5@:J@9��@9�7@9�7@9x�@9%@8��@8Ĝ@8�9@8�@7��@7l�@7\)@7;d@6�y@6�+@6ff@6V@6$�@6$�@6@5�T@5@5��@5�@4��@4�@3ƨ@3S�@2�@2��@2��@2�\@2^5@1�@17L@1%@1%@0��@0��@0��@0�9@0��@0�u@0�@0bN@0bN@0r�@0bN@0 �@/�@/\)@/+@/
=@.�y@.ȴ@.�R@.��@.v�@.V@-�T@-�-@-`B@-/@,��@,�@,I�@+�
@+�@+dZ@+dZ@+dZ@+S�@+C�@+33@*��@*�@)��@)hs@)%@(�9@(1'@(  @'�;@'��@'K�@&��@&ȴ@&��@&E�@%��@%p�@%O�@%?}@%/@%�@$��@$j@$1@#�m@#C�@"�!@"n�@"=q@!�@!�^@!��@!�7@!hs@ ��@ �9@ �@�;@�w@�w@�P@l�@l�@K�@+@�@�+@V@$�@�T@�-@��@/@9X@ƨ@��@t�@S�@C�@"�@�!@=q@�@��@x�@X@�@Ĝ@�@�@Q�@b@�w@;d@
=@ȴ@v�@E�@@��@��@`B@?}@��@�@��@��@Z@1@�
@�F@S�@�@�\@n�@^5@-@�@��@��@hs@7L@�@�`@�9@�@r�@bN@A�@ �@ �@�@��@l�@K�@
=@��@�R@��@�R@�R@��@�+@V@E�@5?@{@�T@@��@�h@�@`B@O�@�@�@�@Z@9X@�@�
@�F@��@�@t�@S�@"�@
�@
��@
��@
��@
��@
�\@
~�@
^5@
-@
�@	�@	��@	x�@	hs@	7L@�`@Ĝ@�u@r�@bN@A�@ �@��@�w@�@�P@\)@�@�y@ȴ@�R@v�@5?@{@��@@��@�@p�@p�@p�@O�@�@V@�@�/@��@��@�j@�@Z@I�@(�@�
@�F@�@S�@C�@o@�@�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114411111111111144111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B�uB�uB�hB�PB�JB�JB�PB�JB�DB�DB�DB�DB�JB�JB�+B� Br�BbNBD�B@�B?}B;dBaHBR�BaHBgmB_;B`BBm�By�Bo�Bl�Bn�Bw�Bn�BcTBW
B\)B@�B49BJB1'B#�B�BB�;B�)BɺBȴB��B�9B�?B�^B�wB�B��B�DBVB\BF�B:^B,B�BB
�B
�fB
��B
ŢB
B
�'B
}�B
%B
.B
{B	�B	�B
B
%B	�fB	�B	�B	�/B	��B	��B	��B	��B	��B	��B	�-B	��B	��B	��B	��B	�=B	�%B	��B	�VB	�1B	�JB	�oB	�oB	�DB	�%B	� B	r�B	VB	_;B	aHB	N�B	8RB	+B	8RB	/B	49B	)�B	�B	�B	"�B	�B	PB��B	�B	�B	�B	{B	%B	PB	oB	bB	DB	%B	B��B��B��B	B��B��B�B�HB��B��B��B��BŢBȴB��B��B�wB��B��BB��B�XB�B��B�B��B�=B�VB�+B~�Bw�BjBiyB|�Bz�Bv�Bz�Bp�B\)BdZBaHBhsBW
BcTB^5BVBM�BC�BE�B;dBF�BP�BJ�BB�BJ�BVBW
BN�BR�BS�BM�BN�BM�BI�BB�B+B%BhB-B)�B!�B�B1B��B!�B�B&�B&�B$�B�B�B�B�B!�B-B$�B$�B&�B%�B$�B�BB�BB�B�BuBB��BPB"�B�B!�B�B�B{BJB�B �B �B�B{BJB�B"�B�B+B\BuB{B)�B)�B&�B+B,B(�B+B6FB49B2-B7LB8RB0!B&�B&�B:^B7LB;dB@�B<jB>wBI�BI�BJ�BF�BG�BK�BM�BF�BE�BO�BM�BK�BM�BS�BW
BXBW
BT�BM�B?}BQ�BS�BP�B[#B^5BaHBe`BiyBdZBiyBcTB\)Bn�Br�Bs�Bs�Bt�Bv�Bx�B}�B� B~�B}�B{�Bw�B{�B�B�DB�PB�PB�=B�%B�B�B�bB��B��B��B��B��B��B�\B��B�?BBŢBɺB��B��B��B��B��B�wB�wB�}B�;B��B��B��B�B�B��B��B��B��B��B��B��B	B	B		7B		7B	
=B	
=B	JB	�B	uB	oB	�B	�B	�B	�B	�B	!�B	$�B	$�B	#�B	!�B	�B	 �B	!�B	 �B	�B	�B	bB	%�B	.B	7LB	9XB	:^B	:^B	:^B	=qB	>wB	=qB	=qB	=qB	@�B	D�B	D�B	D�B	D�B	D�B	D�B	H�B	J�B	I�B	F�B	Q�B	P�B	P�B	S�B	ZB	ZB	[#B	ZB	\)B	\)B	\)B	XB	`BB	bNB	cTB	hsB	iyB	m�B	n�B	n�B	p�B	p�B	r�B	r�B	p�B	k�B	t�B	u�B	~�B	~�B	� B	�JB	�PB	�JB	�JB	�bB	�oB	�uB	�uB	�oB	�uB	�uB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�!B	�'B	�-B	�'B	�3B	�LB	�FB	�FB	�?B	�RB	�LB	�FB	�?B	�3B	�FB	�RB	�RB	�}B	�}B	�wB	�wB	�wB	B	ĜB	ŢB	ŢB	ĜB	ÖB	ÖB	ǮB	ȴB	ȴB	��B	��B	��B	ɺB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�
B	�
B	�B	�
B	�B	�#B	�#B	�B	�B	�B	�/B	�BB	�;B	�BB	�NB	�HB	�ZB	�sB	�yB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
  B
  B
B
  B
B
B
%B
B
B
B
B
B
1B
	7B

=B

=B
	7B

=B
JB
DB

=B
DB
DB
JB
PB
PB
PB
PB
PB
PB
\B
\B
oB
uB
uB
uB
uB
oB
oB
bB
VB
\B
{B
�B
�B
�B
�B
�B
{B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
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
"�B
"�B
"�B
#�B
%�B
'�B
'�B
'�B
'�B
'�B
'�B
'�B
'�B
&�B
'�B
)�B
+B
)�B
'�B
)�B
+B
)�B
(�B
&�B
)�B
,B
,B
+B
,B
-B
.B
.B
.B
.B
.B
.B
-B
+B
,B
-B
)�B
.B
0!B
2-B
2-B
2-B
1'B
0!B
0!B
5?B
6FB
6FB
6FB
6FB
6FB
7LB
7LB
7LB
7LB
8RB
7LB
7LB
5?B
5?B
49B
7LB
8RB
8RB
8RB
9XB
9XB
8RB
8RB
7LB
9XB
8RB
9XB
8RB
:^B
9XB
9XB
;dB
=qB
>wB
=qB
=qB
=qB
;dB
9XB
8RB
;dB
=qB
=qB
=qB
=qB
?}B
@�B
?}B
?}B
@�B
@�B
@�B
@�B
@�B
B�B
C�B
D�B
D�B
C�B
B�B
@�B
A�B
C�B
A�B
B�B
E�B
F�B
F�B
G�B
H�B
H�B
G�B
F�B
G�B
H�B
F�B
J�B
K�B
K�B
K�B
K�B
K�B
J�B
I�B
J�B
K�B
L�B
K�B
L�B
L�B
I�B
H�B
M�B
O�B
P�B
P�B
P�B
O�B
N�B
N�B
Q�B
P�B
R�B
R�B
R�B
R�B
S�B
VB
T�B
S�B
S�B
R�B
VB
T�B
VB
W
B
W
B
XB
XB
XB
YB
YB
ZB
YB
YB
YB
YB
ZB
ZB
YB
ZB
[#B
]/B
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
_;B
`BB
`BB
`BB
`BB
`BB
_;B
_;B
`BB
aHB
aHB
bNB
bNB
cTB
dZB
cTB
cTB
bNB
cTB
cTB
cTB
cTB
cTB
cTB
dZB
dZB
dZB
dZB
dZB
cTB
cTB
cTB
cTB
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
gmB
gmB
iyB
hsB
hsB
hsB
gmB
hsB
hsB
hsB
gmB
hsB
iyB
iyB
hsB
jB
jB
jB
k�B
jB
jB
jB
l�B
l�B
k�B
k�B
k�B
l�B
m�B
m�B
l�B
l�B
m�B
m�B
n�B
o�B
o�B
p�B
p�B
p�B
o�B
n�B
p�B
p�B
p�B
p�B
p�B
p�B
p�B
o�B
p�B
p�B
o�B
q�B
q�B
q�B
r�B
r�B
s�B
t�B
t�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111444111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B�dB�dB�jB�dB�^B�^B�xB�xB�~B�~B��B��BtBdtBHBC�BCGB?Bb�BU�Bb�Bh�BaBb4Bn}BzDBp�Bm�BoiBx8BoOBd�BX�B]dBC-B6�B�B2|B%�BWB�B��B��B�B�DB�B�`B�B�JB��B�!B�G�O�G�O�BBHB<�B-�B!�BB
�IB
�B
ӏB
��B
��B
��G�O�G�O�B
1AB
�B	�zB	��B
�B
�B	�B	��B	یB	�B	�"B	�hB	�.B	��B	̘B	��B	��B	��B	�vB	��B	��B	��B	��B	��B	�vB	�RB	�B	��B	��B	��B	��B	�iB	s�B	X+B	`BB	a�B	PbB	:�B	-CB	9�B	0�B	5?B	+�B	�B	;B	#�B	�B	BB��B	�B	B	9B	MB	�B	B	�B	�B	�B	�B	�B��B��B��B	MB�wB�DB�AB�hBҽB�uBөB��B��BɠB�VB�JB��B�'B� B��B�B�*B��B��B��B��B�0B��B��B��By�Bl�BkkB}�B{�Bw�B{�Bq�B^OBe�BbhBiDBX�Bc�B_!BW$BO\BE9BG+B=VBG�BQ�BK�BD3BK�BVSBWYBO�BS[BTaBN�BO\BN"BJ#BCB,qB	�B�B-�B+B# B#B
�B�B"hB�B'�B'�B%�B�B�B�B�B"�B-]B%�B%�B'mB&�B%FBWB�B�B�B;BIBaB�B��BVB# B \B"NBVBIB�B�BSB!bB!HBWBgB�B \B# BeB�B�B�BB*eB*�B'�B+�B,�B)�B+�B6�B4�B2�B7�B8�B0�B(>B(XB:�B88B<B@�B=<B?.BJ	BJ#BK)BG+BHKBLBNBG_BFYBP.BN<BL~BNVBTaBW?BX+BW?BU2BNVBA BRoBT�BQ�B[�B^�Ba�Be�Bi�BeBi�BdB]dBn�Br�BtBs�BuBwBy$B~(B�4BB~BB|6BxlB|jB��B�^B�jB�jB��B��B��B�9B�B��B�B�B��B�'B�qB�NB�B��B��B�%B�	B�B�B� B�"B�)B��B� B�UB��B��B��B�B�B�-B�zB�>B�B�BB�PB�B�cB	[B	�B		lB		lB	
rB	
rB	~B	gB	�B	�B	�B	�B	�B	�B	�B	!�B	$�B	$�B	$B	!�B	�B	!B	!�B	 �B	B	B	�B	&LB	.cB	7LB	9rB	:xB	:xB	:xB	=qB	>�B	=�B	=�B	=�B	@�B	D�B	D�B	D�B	D�B	D�B	D�B	H�B	J�B	I�B	G+B	RB	QB	QB	T,B	ZB	Z7B	[=B	Z�B	\]B	\xB	\�B	X�B	`�B	b�B	c�B	h�B	i�B	m�B	n�B	n�B	p�B	p�B	r�B	r�B	p�B	l=B	uB	v+B	.B	HB	��B	�JB	�jB	�~B	�~B	�}B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	��B	�'B	�;B	�B	�$B	�KB	�=B	�5B	�cB	�;B	�AB	�GB	�[B	�hB	�fB	�`B	�`B	�tB	�RB	�fB	�zB	��B	��B	�zB	��B	��B	�}B	��B	��B	��B	��B	��B	ĶB	ŢB	ŢB	ĶB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	� B	� B	�B	� B	�B	�B	�B	�B	�B	�B	�$B	�$B	�9B	�?B	�7B	�WB	�=B	�QB	�KB	�kB	�dB	�\B	�pB	�vB	�B	�B	�B	�B	�B	�B	��B	��B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�	B	�B	��B	�B	��B	�B	�	B	�B	�B	�B	�(B
 B
 B
 B
 4B
'B
3B
%B
SB
9B
aB
[B
uB
fB
	RB

XB

XB
	RB

XB
JB
^B

XB
^B
^B
dB
jB
jB
�B
jB
�B
�B
vB
�B
�B
uB
�B
uB
uB
�B
�B
}B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B

B
�B
�B
�B
�B
�B
�B
�B
�B
�B
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
"�B
"�B
#B
$B
&B
'�B
($B
(
B
(
B
($B
(
B
(
B
(
B
'B
(
B
)�B
+B
*B
($B
*B
+B
*0B
)B
'8B
*B
,"B
,"B
+6B
,=B
-CB
.B
./B
.B
./B
.IB
./B
-)B
+6B
,"B
-CB
*B
.IB
0;B
2GB
2GB
2GB
1[B
0UB
0oB
5?B
6FB
6FB
6FB
6`B
6`B
7LB
7LB
7LB
7fB
8RB
7LB
7LB
5ZB
5ZB
4nB
7fB
8lB
8�B
8lB
9XB
9XB
8lB
8lB
7�B
9rB
8lB
9rB
8�B
:xB
9�B
9�B
;B
=qB
>wB
=qB
=qB
=qB
;B
9�B
8�B
;�B
=�B
=�B
=�B
=�B
?�B
@�B
?�B
?�B
@�B
@�B
@�B
@�B
@�B
B�B
C�B
D�B
D�B
C�B
B�B
@�B
A�B
C�B
A�B
B�B
E�B
F�B
F�B
G�B
H�B
H�B
G�B
F�B
G�B
H�B
F�B
J�B
K�B
K�B
K�B
K�B
K�B
J�B
I�B
J�B
K�B
L�B
K�B
L�B
L�B
J	B
IB
M�B
O�B
QB
Q B
Q B
O�B
OB
OB
RB
QB
SB
SB
SB
SB
TB
VB
U2B
TB
TB
S&B
VB
UB
V9B
W$B
W$B
X+B
X+B
X+B
Y1B
Y1B
Z7B
Y1B
YKB
Y1B
Y1B
Z7B
Z7B
YKB
ZQB
[WB
]IB
]IB
]IB
]IB
]IB
^OB
^OB
^OB
^OB
^OB
^jB
_VB
`BB
`BB
`\B
`\B
`BB
_VB
_VB
`vB
abB
abB
bhB
bhB
cTB
dZB
cTB
cTB
b�B
cnB
cTB
cTB
cnB
cnB
cnB
dtB
dtB
dZB
dZB
dZB
cnB
cnB
cnB
cnB
ezB
ezB
ezB
f�B
ffB
f�B
ffB
f�B
ffB
g�B
g�B
g�B
i�B
h�B
hsB
hsB
g�B
h�B
h�B
h�B
g�B
h�B
iyB
i�B
h�B
jB
j�B
j�B
k�B
j�B
j�B
j�B
l�B
l�B
k�B
k�B
k�B
l�B
m�B
m�B
l�B
l�B
m�B
m�B
n�B
o�B
o�B
p�B
p�B
p�B
o�B
n�B
p�B
p�B
p�B
p�B
p�B
p�B
p�B
o�B
p�B
p�B
o�B
q�B
q�B
q�B
r�B
r�B
s�B
t�B
t�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114411111111111144111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oG�O�G�O�;o;o;o;o;o;o;o;o;o;o;o;oG�O�G�O�;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
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
PRES            TEMP            PSAL            PRES            TEMP            PSAL            PRES            TEMP            PSAL            PRES_ADJ=PRES-SP,  where SP is SURFACE PRESSURE from next cycle; PRES_ADJ_ERR=Manufacturer sensor accuracy                                                                                                                                                      TEMP_ADJ=TEMP; TEMP_ADJ_ERR=Manufacturer sensor accuracy                                                                                                                                                                                                        PSAL_ADJ = RecalS= psal(PRES_ADJ,TEMP,Conductivity)                                                                                                                                                                                                             None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            PSAL_ADJ = celltm_sbe41(RecalS,TEMP,P,elapsed_time,alpha,tau); elapsed_time=P/mean_rise_rate; P=dbar since the start of the profile for each samples                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            PSAL_ADJ_ERR=max(RecalS & CTM error, 0.01(PSS-78))                                                                                                                                                                                                              SP=0.02(dbar)                                                                                                                                                                                                                                                   None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            alpha=0.0267C, tau=18.6s, mean_rise_rate = 0.09 dbar/second                                                                                                                                                                                                     None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            Pressure Correction using reported SURFACE PRESSURE                                                                                                                                                                                                             None                                                                                                                                                                                                                                                            Salinity Recalculation using PRES_ADJ                                                                                                                                                                                                                           None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            Correction for conductivity cell thermal mass error(CTM), Johnson et al., 2007, JAOT                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            No adjustment is needed                                                                                                                                                                                                                                         201901190036052019011900360520190119003605201901190200172019011902001720190119020017201901200024332019012000243320190120002433  JA  ARFMdecpA19c                                                                20190115093748  IP                  G�O�G�O�G�O�                JA  ARFMfmtp2.8                                                                 20190115003749  IP                  G�O�G�O�G�O�                JA  ARCAsspa3.1b                                                                20190115003753  IP  PRES            G�O�G�O�G�O�                JA  ARGQrqcppo_c                                                                20190115003753  QCP$                G�O�G�O�G�O�              3CJA  ARGQrqcpt19d                                                                20190115003754  QCP$                G�O�G�O�G�O�           80000JA  ARGQaqcpt19d                                                                20190115003754  QCP$                G�O�G�O�G�O�           80000JA  ARGQrqcp2.8e                                                                20190115003754  QCP$                G�O�G�O�G�O�            FB40JA  ARGQrqcp2.8e                                                                20190115003754  QCF$                G�O�G�O�G�O�            4000JA  ARGQaqcp2.8e                                                                20190115003754  QCP$                G�O�G�O�G�O�            FB40JA  ARGQaqcp2.8e                                                                20190115003754  QCF$                G�O�G�O�G�O�            4000JA  ARGQrqcpt16c                                                                20190115003755  QCP$                G�O�G�O�G�O�           10000JA      jafc1.0                                                                 20190115003755                      G�O�G�O�G�O�                JA  ARUP                                                                        20190115005711                      G�O�G�O�G�O�                JM  ARGQJMQC2.0                                                                 20190115153725  CV  JULD            G�O�G�O�F���                JM  ARGQJMQC2.0                                                                 20190115153725  CV  JULD_LOCATION   G�O�G�O�F���                JM  ARGQJMQC2.0                                                                 20190115153725  CV  LATITUDE        G�O�G�O�A�"�                JM  ARGQJMQC2.0                                                                 20190115153725  CV  LONGITUDE       G�O�G�O��"s�                JM  ARSQJMQC2.0                                                                 20190116000000  CF  PSAL_ADJUSTED_QCC<  CT  G�O�                JM  ARSQJMQC2.0                                                                 20190116000000  CF  TEMP_ADJUSTED_QCC<  CT  G�O�                JM  ARCAJMQC2.0                                                                 20190118153605  CV  PRES_ADJUSTED   G�O�G�O�G�O�                JM  ARCAJMQC2.0                                                                 20190118153605  CV  PSAL_ADJUSTED   G�O�G�O�G�O�                JM  ARCAJMTM1.0                                                                 20190118170017  CV  PSAL_ADJUSTED   G�O�G�O�G�O�                JM  ARSQOW  1.1 2017V1                                                          20190119152433  IP  PSAL_ADJUSTED   G�O�G�O�G�O�                JA  ARDU                                                                        20200115131516                      G�O�G�O�G�O�                