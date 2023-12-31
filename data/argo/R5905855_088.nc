CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       JMA    source        
Argo float     history       E2022-06-04T19:26:10Z creation;2022-06-04T19:26:10Z conversion to V3.1      
references        (http://www.argodatamgt.org/Documentation   comment       	free text      user_manual_version       3.1    Conventions       Argo-3.1 CF-1.6    featureType       trajectoryProfile         @   	DATA_TYPE                  	long_name         	Data type      conventions       Argo reference table 1     
_FillValue                    6�   FORMAT_VERSION                 	long_name         File format version    
_FillValue                    6�   HANDBOOK_VERSION               	long_name         Data handbook version      
_FillValue                    6�   REFERENCE_DATE_TIME                 	long_name         !Date of reference for Julian days      conventions       YYYYMMDDHHMISS     
_FillValue                    6�   DATE_CREATION                   	long_name         Date of file creation      conventions       YYYYMMDDHHMISS     
_FillValue                    6�   DATE_UPDATE                 	long_name         Date of update of this file    conventions       YYYYMMDDHHMISS     
_FillValue                    6�   PLATFORM_NUMBER                   	long_name         Float unique identifier    conventions       WMO float identifier : A9IIIII     
_FillValue                    6�   PROJECT_NAME                  	long_name         Name of the project    
_FillValue                  @  6�   PI_NAME                   	long_name         "Name of the principal investigator     
_FillValue                  @  78   STATION_PARAMETERS           	            	long_name         ,List of available parameters for the station   conventions       Argo reference table 3     
_FillValue                  0  7x   CYCLE_NUMBER               	long_name         Float cycle number     conventions       =0...N, 0 : launch cycle (if exists), 1 : first complete cycle      
_FillValue         ��        7�   	DIRECTION                  	long_name         !Direction of the station profiles      conventions       -A: ascending profiles, D: descending profiles      
_FillValue                    7�   DATA_CENTRE                   	long_name         .Data centre in charge of float data processing     conventions       Argo reference table 4     
_FillValue                    7�   DC_REFERENCE                  	long_name         (Station unique identifier in data centre   conventions       Data centre convention     
_FillValue                     7�   DATA_STATE_INDICATOR                  	long_name         1Degree of processing the data have passed through      conventions       Argo reference table 6     
_FillValue                    7�   	DATA_MODE                  	long_name         Delayed mode or real time data     conventions       >R : real time; D : delayed mode; A : real time with adjustment     
_FillValue                    7�   PLATFORM_TYPE                     	long_name         Type of float      conventions       Argo reference table 23    
_FillValue                     7�   FLOAT_SERIAL_NO                   	long_name         Serial number of the float     
_FillValue                     7�   FIRMWARE_VERSION                  	long_name         Instrument firmware version    
_FillValue                     8   WMO_INST_TYPE                     	long_name         Coded instrument type      conventions       Argo reference table 8     
_FillValue                    8<   JULD               standard_name         time   	long_name         ?Julian day (UTC) of the station relative to REFERENCE_DATE_TIME    units         "days since 1950-01-01 00:00:00 UTC     conventions       8Relative julian days with decimal part (as parts of day)   
resolution        >�����h�   
_FillValue        A.�~       axis      T           8@   JULD_QC                	long_name         Quality on date and time   conventions       Argo reference table 2     
_FillValue                    8H   JULD_LOCATION                  	long_name         @Julian day (UTC) of the location relative to REFERENCE_DATE_TIME   units         "days since 1950-01-01 00:00:00 UTC     conventions       8Relative julian days with decimal part (as parts of day)   
resolution        >�����h�   
_FillValue        A.�~            8L   LATITUDE               	long_name         &Latitude of the station, best estimate     standard_name         latitude   units         degree_north   
_FillValue        @�i�       	valid_min         �V�        	valid_max         @V�        axis      Y           8T   	LONGITUDE                  	long_name         'Longitude of the station, best estimate    standard_name         	longitude      units         degree_east    
_FillValue        @�i�       	valid_min         �f�        	valid_max         @f�        axis      X           8\   POSITION_QC                	long_name         ,Quality on position (latitude and longitude)   conventions       Argo reference table 2     
_FillValue                    8d   POSITIONING_SYSTEM                    	long_name         Positioning system     
_FillValue                    8h   PROFILE_PRES_QC                	long_name         #Global quality flag of PRES profile    conventions       Argo reference table 2a    
_FillValue                    8p   PROFILE_TEMP_QC                	long_name         #Global quality flag of TEMP profile    conventions       Argo reference table 2a    
_FillValue                    8t   PROFILE_PSAL_QC                	long_name         #Global quality flag of PSAL profile    conventions       Argo reference table 2a    
_FillValue                    8x   VERTICAL_SAMPLING_SCHEME                  	long_name         Vertical sampling scheme   conventions       Argo reference table 16    
_FillValue                    8|   CONFIG_MISSION_NUMBER                  	long_name         :Unique number denoting the missions performed by the float     conventions       !1...N, 1 : first complete mission      
_FillValue         ��        9|   PRES         
      
   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���   axis      Z        �  9�   PRES_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  I@   PRES_ADJUSTED            
      	   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  M0   PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  \�   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     
_FillValue        G�O�   	valid_min         �      	valid_max         B      C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  `�   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  p�   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     
_FillValue        G�O�   	valid_min         �      	valid_max         B      C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  t�   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �P   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    
_FillValue        G�O�   	valid_min         @      	valid_max         B$     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  �@   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �    PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    
_FillValue        G�O�   	valid_min         @      	valid_max         B$     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  ��   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ��   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         decibar    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  ��   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         degree_Celsius     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  �`   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  �    	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  0  ��   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                    �   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                    �   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                    �   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  ,  �   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                    �<   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                    �@   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                    �D   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                    �H   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  @  �L   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                    �   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                    �   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    �   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar         �   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar         �   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�        �   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    �Argo profile    3.1 1.2 19500101000000  20220604192610  20220610161505  5905855                                                                 JAMSTEC                                                         PRES            TEMP            PSAL               XA   JA                                  2B  A   APEX                            8423                            2.11.2                          846 @ق�R��d1   @ق���@*��hr�!�d��E�1   GPS     A   A   A   Primary sampling: averaged [2 dbar bin average for 1 Hz CTD]                                                                                                                                                                                                       @��@�  @�  A   A   A@  A`  A�  A�  A�  A�  A�  A�  A�  A�  B   B  B  B  B   B(  B0  B8  B@  BH  BP  BXffB`  Bh  Bp  Bx  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�33B�  B�33B���B���B�  B�  B�  B�  B�  B�ffBߙ�B�  B�  B�  B�  B�  B�  B�  C   C  C  C�C��C	�fC  C�fC  C  C�fC  C  C  C  C  C   C"  C$  C&  C(  C*  C,33C.  C0  C2  C4  C6  C8  C9�fC<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^�C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C��C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� DfD� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP�fDQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dt� Du  Du� Dv  Dv� Dw  Dw� Dx  Dx� Dy  Dy� Dz  Dz� D{  D{� D|  D|� D}  D}� D~  D~� D  D� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D D�� D�  D�@ DÀ D�� D�  D�@ DĀ D�� D�  D�@ Dŀ D�� D�  D�@ Dƀ D�� D�  D�@ Dǃ3D��3D�  D�@ DȀ D�� D�  D�@ Dɀ D�� D�  D�@ Dʀ D�� D�  D�@ Dˀ D�� D�  D�@ D̀ D�� D�  D�@ D̀ D�� D�  D�@ D΀ D�� D�  D�@ Dπ D�� D�  D�@ DЀ D�� D�  D�@ Dр D�� D�  D�@ DҀ D�� D�  D�@ DӀ D�� D�  D�@ DԀ D�� D�  D�@ DՀ D�� D�  D�@ Dր D�� D�  D�@ D׀ D�� D�  D�@ D؀ D�� D�  D�@ Dـ D�� D�  D�@ Dڀ D�� D�  D�@ Dۃ3D��3D�  D�@ D܀ D�� D�  D�@ D݀ D�� D�  D�@ Dހ D�� D�  D�@ D߀ D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D��f111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111@Q�@~�R@�\)@�\)A�A?�A_�A�A��
A��
A��
A��
A��
A��
A��
A��
B�B�B�B�B'�B/�B7�B?�BG�BO�BXQ�B_�Bg�Bo�Bw�B�B���B���B���B���B���B���B���B���B���B���B���B���B�(�B���B�(�B�B�B���B���B���B���B���B�\)Bߏ]B���B���B���B���B���B���B���B���C��C��C{CǮC	�GC��C�GC��C��C�GC��C��C��C��C��C��C!��C#��C%��C'��C)��C,.C-��C/��C1��C3��C5��C7��C9�GC;��C=��C?��CA��CC��CE��CG��CI��CK��CM��CO��CQ��CS��CU��CW��CY��C[��C^{C_��Ca��Cc��Ce��Cg��Ci��Ck��Cm��Co��Cq��Cs��Cu��Cw��Cy��C{��C}��C��C��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC�
>C�
>C�
>C��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC�
>C��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qD ~�D ��D~�D��D~�DD~�D��D~�D��D~�D��D~�D��D~�D��D~�D��D	~�D	��D
~�D
��D~�D��D~�D��D~�D��D~�D��D~�D��D~�D��D~�D��D~�D��D~�D��D~�D��D~�D��D~�D��D~�D��D~�D��D~�D��D~�D��D~�D��D~�D��D~�D��D~�D��D~�D��D ~�D ��D!~�D!��D"~�D"��D#~�D#��D$~�D$��D%~�D%��D&~�D&��D'~�D'��D(~�D(��D)~�D)��D*~�D*��D+~�D+��D,~�D,��D-~�D-��D.~�D.��D/~�D/��D0~�D0��D1~�D1��D2~�D2��D3~�D3��D4~�D4��D5~�D5��D6~�D6��D7~�D7��D8~�D8��D9~�D9��D:~�D:��D;~�D;��D<~�D<��D=~�D=��D>~�D>��D?~�D?��D@~�D@��DA~�DA��DB~�DB��DC~�DC��DD~�DD��DE~�DE��DF~�DF��DG~�DG��DH~�DH��DI~�DI��DJ~�DJ��DK~�DK��DL~�DL��DM~�DM��DN~�DN��DO~�DO��DP�DP��DQ~�DQ��DR~�DR��DS~�DS��DT~�DT��DU~�DU��DV~�DV��DW~�DW��DX~�DX��DY~�DY��DZ~�DZ��D[~�D[��D\~�D\��D]~�D]��D^~�D^��D_~�D_��D`~�D`��Da~�Da��Db~�Db��Dc~�Dc��Dd~�Dd��De~�De��Df~�Df��Dg~�Dg��Dh~�Dh��Di~�Di��Dj~�Dj��Dk~�Dk��Dl~�Dl��Dm~�Dm��Dn~�Dn��Do~�Do��Dp~�Dp��Dq~�Dq��Dr~�Dr��Ds~�Ds��Dt~�Dt��Du~�Du��Dv~�Dv��Dw~�Dw��Dx~�Dx��Dy~�Dy��Dz~�Dz��D{~�D{��D|~�D|��D}~�D}��D~~�D~��D~�D��D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D¿\D��\D�?\D�\Dÿ\D��\D�?\D�\DĿ\D��\D�?\D�\Dſ\D��\D�?\D�\Dƿ\D��\D�?\Dǂ�D�D��\D�?\D�\Dȿ\D��\D�?\D�\Dɿ\D��\D�?\D�\Dʿ\D��\D�?\D�\D˿\D��\D�?\D�\D̿\D��\D�?\D�\DͿ\D��\D�?\D�\Dο\D��\D�?\D�\DϿ\D��\D�?\D�\Dп\D��\D�?\D�\Dѿ\D��\D�?\D�\Dҿ\D��\D�?\D�\Dӿ\D��\D�?\D�\DԿ\D��\D�?\D�\Dտ\D��\D�?\D�\Dֿ\D��\D�?\D�\D׿\D��\D�?\D�\Dؿ\D��\D�?\D�\Dٿ\D��\D�?\D�\Dڿ\D��\D�?\Dۂ�D�D��\D�?\D�\Dܿ\D��\D�?\D�\Dݿ\D��\D�?\D�\D޿\D��\D�?\D�\D߿\D��\D�?\D�\D�\D��\D�?\D�\D�\D��\D�?\D�\D�\D��\D�?\D�\D�\D��\D�?\D�\D�\D��\D�?\D�\D�\D��\D�?\D�\D�\D��\D�?\D�\D�\D��\D�?\D�\D�\D��\D�?\D�\D�\D��\D�?\D�\D�\D��\D�?\D�\D�\D��\D�?\D�\D�\D��\D�?\D�\D��\D��\D�?\D�\D�\D��\D�?\D�\D�\D��\D�?\D�\D�\D��\D�?\D�\D�\D��\D�?\D�\D�\D��\D�?\D�\D�\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D���111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111A��JA� 4A�+A�~A�"A��A�PA��A�VA��A�.A�.A�bA�bA��A�:A�JA�
�A�bA��A��A�
=A��8A���AѶFAѣ�A�xlA�2�A��dAΛ�AΎ�A΄�A�[�A�LdA�N<A�K)AͿHA̡-A�|�A���Aȓ@A�A���A�t�A��A��A���A�o5A���A�"�A��A�
rA���A���A���A�7A��2A�n/A�՛A�`�A���A�t�A��'A���A��NA���A�R�A��A���A���A�E�A��A�^5A���A�MA��A�o5A���A�ʌA�9�A�F�A��A�ѷA�qvA�k�A�\)A�YA�e�A��A~��A|�vAz�FAy��At�An\)Aj�Ah)�Agq�Ad�tA]$tAZ�FAX�AVb�AQ�ANOAL��AJ�AIh�AFo�AB��AAHA<��A;7�A:A A91�A7�[A6QA5ZA5A4�[A47A3��A30UA2��A2S�A1��A1�rA1��A1 �A0��A0�}A/�A.MA-l�A,�A*W�A)��A(U2A(($A'�2A'�	A'HA'�A&�jA&t�A&OA%e,A$��A$[�A#��A#�AA#I�A"�>A"��A"1�A!�IA!	A ��A��A�IA�-Ap�A��A�AOA�VA<6A�mA��A,=A6zA��Au%AVA!�A�A�AA�A�gA]�A��As�ARTAA�0A��A�.Ag8Ae�AW�AZ�A/AGA�6A`�A@A�A��A�7A?A�A��A-�Ay>A6A�QA��A\)A�>A��A�A"�A��A��A}�AIRAߤA�{AG�A �A
��A
t�A	��A	��A	�A�hArGA"�A��A��Aa�Am]A�/A��A*0A��A��Aa|A�A��A��A(�Ac�A;�A'�AGA��A�4AV�A,=A �|A ĜA Vm@�&�@���@�:�@��X@��)@��P@��@���@�s@���@��{@��`@�u@���@�O�@��@��H@�I�@�0U@�@�o @�.@�qv@��m@�I�@︻@��@���@��W@��@�<@�.@��D@��@�h�@�/�@�ff@�@��@�ѷ@�@�7@��?@䉠@�W�@�>B@��@㇔@��v@�m�@�@���@ߘ�@���@�r�@�c�@ܭ�@�Z�@�7@��z@ٖS@�
=@؊r@�\)@�d�@��K@�U�@��@Գh@�E�@���@���@���@��@��@Ͻ�@��@Μx@�`�@��@͗$@�C�@��@̿�@�V�@��@ˁ�@��@ʖ�@�$�@ɖS@�iD@��@��@ȧ�@�y>@�Ft@��@���@ǩ*@�T�@�#�@�|�@��}@�
=@Č�@�L0@��@Úk@�Mj@�-w@��@�ں@»�@�H�@��g@�F@�͟@�R�@�خ@��V@�P�@�+@�Ɇ@���@�l"@��m@��@��1@�<�@��Q@��S@�|�@��;@�'�@��}@��.@�E�@���@��@�~�@�%�@�m]@�F�@��M@��O@�K^@���@�@���@���@�L0@���@���@��@��m@�O@��k@�S&@�,�@��'@�Ov@���@�J#@��F@�5?@���@�=�@��M@���@��@��*@�@@�q@�-�@��W@���@�8�@��/@�� @�H�@��;@���@��@��,@��I@�2�@��>@��H@���@�S�@��@���@��U@��.@�/�@��d@�v`@��@���@�Ft@���@�|�@�/@���@�z�@� �@���@�&@�GE@��-@��"@�\)@�!-@���@��@�a@��@��@�d�@�=q@�0U@��@�IR@��h@���@�tT@�B[@� �@�_@��D@��)@��3@���@��v@�l"@�R�@�5?@�	�@��@���@��@��F@�N�@�	@�l�@�J�@�A�@�!-@��1@�i�@��A@�8�@��@���@�G@��@�\�@���@�|�@�g8@�;�@�u@�L�@��@�҉@��_@�Ov@��@���@�p�@�/@��p@�?@��@�1@��@��@���@��p@�]d@�)�@���@��a@��n@�t�@�j�@�g�@�RT@�-w@��@���@���@�c�@�=q@�@��@���@�qv@�4�@��8@���@��A@�K^@��A@���@�m]@��@��/@��@���@�d�@�?@�r@C@~�+@~H�@~;�@~�@}�@|�@|bN@{�K@{RT@{$t@z�,@z8�@y�H@y��@y�@x�@w��@w�@w@vQ@v{@u�T@u��@ue,@u@t��@tV�@s��@sdZ@r��@r@q��@q/@p��@p��@p��@p�@pr�@pG@oiD@n��@n�F@n_�@m��@m�=@mN<@l�P@l�O@l|�@lFt@k�&@kA�@k
=@j��@jZ�@jB[@j�@i7L@hr�@h�@h@g��@g�@fu%@f$�@f	@e�3@eG�@dی@d�4@d�@d/�@c��@c@O@cY@b{�@bGE@a��@aa�@`e�@_˒@_6z@^�@^��@^E�@]�#@](�@\<�@[��@[�{@Z�r@Zq�@ZV@Z8�@Y��@X�|@X�@X]d@X�@W˒@W�[@W4�@V}V@V�@V�@U�d@U�~@US&@U8�@U%F@U�@T�K@S��@S9�@R��@R5?@Ru@Q��@Q@P�@P�4@Pr�@PM@P<�@P"h@Px@O� @O{J@OO@O/�@N��@NOv@N�@M�@M%F@L��@L�_@Lj@L�@LG@K�+@K�*@J��@I�@I��@I��@I�"@Ik�@I+�@I�@H��@H��@G��@G��@G�@Gn/@GF�@F��@F~�@F_@E�@EVm@D�K@D�4@D�Y@DM@D1'@D�@C�m@C��@C�{@C�@Bc @A��@A�d@Ax�@A-w@@�@@�@@?�@@x@?��@?�:@?;d@>�2@>xl@>Z�@>M�@>B[@>!�@=�@=�H@=:�@<�5@<�v@<ی@<��@<��@<��@<U2@<G@;s@;;d@:��@:�'@:�F@:YK@:+k@: �@9ϫ@9��@9?}@9�@8�@8�p@8��@8��@7�@7\)@6��@5�@5��@5hs@4��@4�4@4��@4��@4`�@41@3�;@3��@3��@3.I@2�!@2Z�@26�@1�>@1��@1Vm@0��@0��@09X@0  @0�@/�g@/|�@.��@.��@.J�@-�@-��@-F@-�@,��@,��@,*�@+��@+��@+��@+qv@+�@*�c@*��@*a|@*e@)��@)�@)#�@)�@(�`@(��@(�@(H@( �@(1@'�m@'�[@'x@')_@&��@&��@&V@&!�@%��@%�@%��@%a�@$�@$�@$2�@$	�@#�A@#�6@#{J@"ں@"�!@"�r@"W�@":*@"!�@!�3@!\�@!5�@!�@!@ �@ �@ H@ �@�@��@��@��@O@6z@�@�h@{�@J�@1�@�@��@��@�N@��@(�@�@�	@�@�4@~(@bN@M@�@K�@��@�!@�@q�@YK@J�@E�@+k@��@�^@��@��@�n@��@e,@�@�K@�4@[�@M@��@��@��@s@dZ@W?@@O@�@��@��@��@�r@p;@GE@5?@8�@-@�@��@�#@��@L�@�@�@��@N�@!@�m@��@�*@��@�@��@��@YK@5?@�@��@�C@zx@a�@2a@�@�@�@��@Q�@6@  @�k@�{@P�@+@S@�2@ں@��@i�@@�@!�@@�@��@��@��@�n@��@�"@o @B�@%F@�@�5@tT@U2@M@ �@�@��@��@n/@
��@
҉@
��@
��111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111A��JA� 4A�+A�~A�"A��A�PA��A�VA��A�.A�.A�bA�bA��A�:A�JA�
�A�bA��A��A�
=A��8A���AѶFAѣ�A�xlA�2�A��dAΛ�AΎ�A΄�A�[�A�LdA�N<A�K)AͿHA̡-A�|�A���Aȓ@A�A���A�t�A��A��A���A�o5A���A�"�A��A�
rA���A���A���A�7A��2A�n/A�՛A�`�A���A�t�A��'A���A��NA���A�R�A��A���A���A�E�A��A�^5A���A�MA��A�o5A���A�ʌA�9�A�F�A��A�ѷA�qvA�k�A�\)A�YA�e�A��A~��A|�vAz�FAy��At�An\)Aj�Ah)�Agq�Ad�tA]$tAZ�FAX�AVb�AQ�ANOAL��AJ�AIh�AFo�AB��AAHA<��A;7�A:A A91�A7�[A6QA5ZA5A4�[A47A3��A30UA2��A2S�A1��A1�rA1��A1 �A0��A0�}A/�A.MA-l�A,�A*W�A)��A(U2A(($A'�2A'�	A'HA'�A&�jA&t�A&OA%e,A$��A$[�A#��A#�AA#I�A"�>A"��A"1�A!�IA!	A ��A��A�IA�-Ap�A��A�AOA�VA<6A�mA��A,=A6zA��Au%AVA!�A�A�AA�A�gA]�A��As�ARTAA�0A��A�.Ag8Ae�AW�AZ�A/AGA�6A`�A@A�A��A�7A?A�A��A-�Ay>A6A�QA��A\)A�>A��A�A"�A��A��A}�AIRAߤA�{AG�A �A
��A
t�A	��A	��A	�A�hArGA"�A��A��Aa�Am]A�/A��A*0A��A��Aa|A�A��A��A(�Ac�A;�A'�AGA��A�4AV�A,=A �|A ĜA Vm@�&�@���@�:�@��X@��)@��P@��@���@�s@���@��{@��`@�u@���@�O�@��@��H@�I�@�0U@�@�o @�.@�qv@��m@�I�@︻@��@���@��W@��@�<@�.@��D@��@�h�@�/�@�ff@�@��@�ѷ@�@�7@��?@䉠@�W�@�>B@��@㇔@��v@�m�@�@���@ߘ�@���@�r�@�c�@ܭ�@�Z�@�7@��z@ٖS@�
=@؊r@�\)@�d�@��K@�U�@��@Գh@�E�@���@���@���@��@��@Ͻ�@��@Μx@�`�@��@͗$@�C�@��@̿�@�V�@��@ˁ�@��@ʖ�@�$�@ɖS@�iD@��@��@ȧ�@�y>@�Ft@��@���@ǩ*@�T�@�#�@�|�@��}@�
=@Č�@�L0@��@Úk@�Mj@�-w@��@�ں@»�@�H�@��g@�F@�͟@�R�@�خ@��V@�P�@�+@�Ɇ@���@�l"@��m@��@��1@�<�@��Q@��S@�|�@��;@�'�@��}@��.@�E�@���@��@�~�@�%�@�m]@�F�@��M@��O@�K^@���@�@���@���@�L0@���@���@��@��m@�O@��k@�S&@�,�@��'@�Ov@���@�J#@��F@�5?@���@�=�@��M@���@��@��*@�@@�q@�-�@��W@���@�8�@��/@�� @�H�@��;@���@��@��,@��I@�2�@��>@��H@���@�S�@��@���@��U@��.@�/�@��d@�v`@��@���@�Ft@���@�|�@�/@���@�z�@� �@���@�&@�GE@��-@��"@�\)@�!-@���@��@�a@��@��@�d�@�=q@�0U@��@�IR@��h@���@�tT@�B[@� �@�_@��D@��)@��3@���@��v@�l"@�R�@�5?@�	�@��@���@��@��F@�N�@�	@�l�@�J�@�A�@�!-@��1@�i�@��A@�8�@��@���@�G@��@�\�@���@�|�@�g8@�;�@�u@�L�@��@�҉@��_@�Ov@��@���@�p�@�/@��p@�?@��@�1@��@��@���@��p@�]d@�)�@���@��a@��n@�t�@�j�@�g�@�RT@�-w@��@���@���@�c�@�=q@�@��@���@�qv@�4�@��8@���@��A@�K^@��A@���@�m]@��@��/@��@���@�d�@�?@�r@C@~�+@~H�@~;�@~�@}�@|�@|bN@{�K@{RT@{$t@z�,@z8�@y�H@y��@y�@x�@w��@w�@w@vQ@v{@u�T@u��@ue,@u@t��@tV�@s��@sdZ@r��@r@q��@q/@p��@p��@p��@p�@pr�@pG@oiD@n��@n�F@n_�@m��@m�=@mN<@l�P@l�O@l|�@lFt@k�&@kA�@k
=@j��@jZ�@jB[@j�@i7L@hr�@h�@h@g��@g�@fu%@f$�@f	@e�3@eG�@dی@d�4@d�@d/�@c��@c@O@cY@b{�@bGE@a��@aa�@`e�@_˒@_6z@^�@^��@^E�@]�#@](�@\<�@[��@[�{@Z�r@Zq�@ZV@Z8�@Y��@X�|@X�@X]d@X�@W˒@W�[@W4�@V}V@V�@V�@U�d@U�~@US&@U8�@U%F@U�@T�K@S��@S9�@R��@R5?@Ru@Q��@Q@P�@P�4@Pr�@PM@P<�@P"h@Px@O� @O{J@OO@O/�@N��@NOv@N�@M�@M%F@L��@L�_@Lj@L�@LG@K�+@K�*@J��@I�@I��@I��@I�"@Ik�@I+�@I�@H��@H��@G��@G��@G�@Gn/@GF�@F��@F~�@F_@E�@EVm@D�K@D�4@D�Y@DM@D1'@D�@C�m@C��@C�{@C�@Bc @A��@A�d@Ax�@A-w@@�@@�@@?�@@x@?��@?�:@?;d@>�2@>xl@>Z�@>M�@>B[@>!�@=�@=�H@=:�@<�5@<�v@<ی@<��@<��@<��@<U2@<G@;s@;;d@:��@:�'@:�F@:YK@:+k@: �@9ϫ@9��@9?}@9�@8�@8�p@8��@8��@7�@7\)@6��@5�@5��@5hs@4��@4�4@4��@4��@4`�@41@3�;@3��@3��@3.I@2�!@2Z�@26�@1�>@1��@1Vm@0��@0��@09X@0  @0�@/�g@/|�@.��@.��@.J�@-�@-��@-F@-�@,��@,��@,*�@+��@+��@+��@+qv@+�@*�c@*��@*a|@*e@)��@)�@)#�@)�@(�`@(��@(�@(H@( �@(1@'�m@'�[@'x@')_@&��@&��@&V@&!�@%��@%�@%��@%a�@$�@$�@$2�@$	�@#�A@#�6@#{J@"ں@"�!@"�r@"W�@":*@"!�@!�3@!\�@!5�@!�@!@ �@ �@ H@ �@�@��@��@��@O@6z@�@�h@{�@J�@1�@�@��@��@�N@��@(�@�@�	@�@�4@~(@bN@M@�@K�@��@�!@�@q�@YK@J�@E�@+k@��@�^@��@��@�n@��@e,@�@�K@�4@[�@M@��@��@��@s@dZ@W?@@O@�@��@��@��@�r@p;@GE@5?@8�@-@�@��@�#@��@L�@�@�@��@N�@!@�m@��@�*@��@�@��@��@YK@5?@�@��@�C@zx@a�@2a@�@�@�@��@Q�@6@  @�k@�{@P�@+@S@�2@ں@��@i�@@�@!�@@�@��@��@��@�n@��@�"@o @B�@%F@�@�5@tT@U2@M@ �@�@��@��@n/@
��@
҉@
��@
��111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111B
S&B
S&B
SB
S&B
SB
S@B
S@B
S&B
S@B
S@B
S@B
S@B
S[B
S@B
S[B
S&B
S@B
S&B
S&B
S@B
SB
SB
R�B
R:B
Q�B
Q4B
O\B
EB
9�B
:�B
=B
=�B
;�B
<�B
=VB
?�B
@�B
DB
M6B
e�B
��B
��B
�OB
�*B
�;B
�)B
�=BB?HBX�Bb�Bi�B{B�AB��B��B��B��BB��B�gB��B��B�9B��B�jB��BĶB�RB��B�BBn}BS@B>BB,�B5�B-�B�B�B�B
��B
�jB
�fB
��B
��B
e,B
NpB
-�B
�B
B
0B	��B	��B	�\B	�B	�]B	�KB	�4B	}qB	[�B	H�B	>wB	0!B	B	zB�BB�B�B�JB�2B�B�VB	JB	�B	�B	+�B	?cB	G�B	J�B	OBB	V9B	ZQB	]IB	bhB	��B	��B	�@B	�B	��B	�jB
�B
[B
=B
VB
 vB
�B
�B
�B
 \B
'�B
+kB
-]B
/�B
1[B
2aB
3hB
6�B
8lB
>B
@OB
@4B
AB
B�B
CGB
GzB
I�B
I�B
G�B
B[B
>�B
=VB
A B
@�B
?}B
=�B
;0B
:B
6B
3�B
9�B
?�B
@OB
?HB
>�B
=�B
;�B
<jB
;�B
;�B
:*B
9�B
8RB
7�B
7�B
:^B
>BB
=qB
<�B
>�B
@�B
B�B
C-B
BAB
BAB
BAB
CGB
C{B
C�B
B�B
AoB
AoB
AUB
?�B
?cB
=qB
;dB
9�B
3MB
3B
3�B
2�B
1�B
0;B
/OB
.}B
/ B
-�B
-�B
,=B
+�B
*�B
)DB
(�B
$�B
#B
 �B
 \B
�B
�B
�B
�B
yB
�B
�B
[B
TB
�B
�B
�B
TB
�B
�B
B
VB
�B
�B
NB
�B
�B
�B
�B
�B
�B
�B
xB
B
�B
�B
�B
0B
	B
�B
�B
�B
B
B
MB
B
�B
�B
DB
�B
�B
�B
4B
�B
HB
�B
\B
�B
"B
�B
B
�B
~B
JB
xB
B

#B
	�B
	B
�B
1B
�B
KB
EB
EB
B
�B
?B
�B
�B
�B
�B
SB
B
�B
GB
�B
�B
�B
{B
�B
[B
'B
�B
�B
oB
;B
oB
oB
UB
�B
oB
�B
�B
[B
�B
oB
�B
�B
�B
�B
'B
[B
'B
�B
�B
;B
B
 �B
;B
;B
;B
 B
B
 B
B
 �B
B
 B
B
B
B
 �B
 iB	��B	��B	��B	�}B	��B
 �B
 B
;B
 B
oB
oB
�B
 B
 iB	�}B	�cB	�}B	��B	��B
 4B
 �B
;B
�B
[B
AB
'B
�B
�B
�B
B
AB
�B
-B
�B
�B
�B
�B
�B
3B
�B
�B
�B
3B
�B
�B
�B
YB
%B
�B
zB
�B
1B
�B
	RB
	�B

#B

=B

XB

�B
^B
DB
�B
JB
B
jB
�B
�B
<B
�B
B
�B
�B
�B
�B
}B
�B
�B
�B
4B
hB
�B
�B
�B
�B
�B
�B
�B
�B
 B
 B
TB
oB
B
�B
&B
[B
FB
,B
�B
�B
2B
�B
MB
�B
�B
B
B
�B
�B
�B
�B
YB
yB
�B
1B
1B
B
B
eB
B
�B
�B
�B
�B
CB
CB
CB
B
B
)B
xB
�B
B
B
B
OB
B
�B
VB
�B
 'B
 'B
!|B
!HB
!-B
!bB
"4B
!�B
"�B
#nB
#�B
#�B
$�B
$�B
$�B
%FB
%�B
%�B
%�B
%�B
&�B
&�B
'8B
'�B
'�B
(XB
(�B
)*B
)*B
)�B
*B
*�B
*�B
*�B
*�B
*eB
,"B
,=B
,�B
,�B
-)B
-]B
-�B
-�B
-�B
-�B
.B
-�B
.}B
.�B
/OB
/iB
/�B
/�B
/�B
0oB
0�B
0�B
1[B
1vB
1�B
2-B
2|B
2�B
3hB
3MB
3�B
3�B
3�B
3�B
3�B
49B
4�B
4�B
4�B
4�B
4�B
5ZB
5�B
6B
6+B
6FB
6zB
7B
6�B
6�B
6�B
6�B
6�B
6�B
72B
7�B
7�B
7�B
7�B
7�B
8B
8RB
8�B
9$B
9	B
:B
:xB
:xB
:�B
;0B
;JB
;0B
;dB
;dB
;�B
<�B
<�B
=B
<�B
=VB
=�B
>B
>(B
>]B
>�B
>wB
>�B
?HB
?HB
?}B
?�B
?�B
?�B
@�B
A;B
AUB
A;B
A;B
A�B
B�B
B�B
B�B
B�B
CB
C{B
C�B
C�B
C�B
DgB
D�B
D�B
EmB
ESB
EmB
E�B
F?B
F�B
G_B
G�B
G�B
G�B
G�B
H�B
H�B
IB
I�B
J#B
J	B
I�B
I�B
JrB
J�B
J�B
KB
K^B
KDB
K)B
K�B
LB
LJB
LdB
L~B
L�B
L�B
L�B
L�B
L�B
L�B
M�B
M�B
N�B
N�B
N�B
O(B
O�B
O�B
PB
P.B
PHB
PHB
P}B
P}B
P�B
P�B
Q B
QB
Q�B
Q�B
RB
RB
R�B
S@B
S@B
SuB
S�B
S�B
S�B
S�B
T�B
U�B
U�B
U�B
VB
VSB
V�B
V�B
V�B
W
B
W�B
W�B
W�B
W�B
W�B
XB
X_B
X�B
X�B
X�B
YeB
YB
YB
Y�B
Y�B
ZB
ZB
ZB
ZQB
Z�B
[WB
[�B
[�B
[�B
\CB
\xB
\�B
]dB
]�B
^5B
^�B
^�B
_;B
_VB
_pB
_VB
_pB
_�B
_�B
_�B
`\B
`�B
`�B
`vB
`�B
`�B
`�B
`�B
aB
a-B
abB
a|B
a�B
a�B
b4B
bNB
b�B
b�B
b�B
c B
cTB
cnB
cTB
cTB
cnB
cB
b�B
bhB
b�B
b�B
b�B
c�B
c�B
c�B
c�B
c�B
d&B
d&B
dB
dB
d@B
d�B
d�B
d�B
eB
eB
ezB
e�B
ffB
ffB
f�B
f�B
gRB
gmB
h
B
hXB
hXB
h�B
h�B
i_B
i�B
i�B
i�B
jB
jKB
jKB
jKB
jB
j�B
j�B
k6B
k6B
kkB
k�B
k�B
l"B
l"B
l=B
l=B
lqB
l�B
l�B
l�B
l�B
mB
m)B
mwB
m�B
nIB
ncB
n}B
n}B
n}B
n�B
oB
o�B
p!B
p�B
p�B
p�B
p�B
p�B
q�B
q�B
q�B
rB
rB
rB
r�B
r�B
r�B
s3B
sB
shB
s�B
s�B
tB
tB
tTB
t�B
t�B
t�B
t�B
t�B
t�B
u%B
u?B
uZB
utB
u�B
u�B
u�B
vB
v`B
v`B
vFB
v+B
v�B
v�B
v�B
wB
v�B
wfB
w�B
w�B
xB
xB
x8B
xRB
xB
x8B
xlB
x�B
x�B
x�B
yXB
yrB
y�B
y�B
y�B
z*B
z^B
zxB
z�B
z�B
z�B
z�B
z�B
z�B
z�B
z�B
{0B
{JB
{B
{�B
{�B
|B
|B
|B
|B
|PB
|PB
|jB
|�B
|�B
|�B
|�B
|�B
}B
}B
}VB
}�B
}qB
}qB
}qB
}�B
~(B
~]B
~wB
~wB
~�B
B
.B
cB
�B
�B
�B
�B
�B
��B
��B
��B
�B
�;B
��B
��B
�B
�AB
�AB
��B
��B
��B
��B
�B
�-B
�GB
�aB
�{B
�aB
�aB
�{B
��B
��B
�B
�B
�3B
�3B
�3B
��B
�9B
�mB
�mB
��B
�mB
�B
�SB
��B
��111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111B
S&B
S&B
SB
S&B
SB
S@B
S@B
S&B
S@B
S@B
S@B
S@B
S[B
S@B
S[B
S&B
S@B
S&B
S&B
S@B
SB
SB
R�B
R:B
Q�B
Q4B
O\B
EB
9�B
:�B
=B
=�B
;�B
<�B
=VB
?�B
@�B
DB
M6B
e�B
��B
��B
�OB
�*B
�;B
�)B
�=BB?HBX�Bb�Bi�B{B�AB��B��B��B��BB��B�gB��B��B�9B��B�jB��BĶB�RB��B�BBn}BS@B>BB,�B5�B-�B�B�B�B
��B
�jB
�fB
��B
��B
e,B
NpB
-�B
�B
B
0B	��B	��B	�\B	�B	�]B	�KB	�4B	}qB	[�B	H�B	>wB	0!B	B	zB�BB�B�B�JB�2B�B�VB	JB	�B	�B	+�B	?cB	G�B	J�B	OBB	V9B	ZQB	]IB	bhB	��B	��B	�@B	�B	��B	�jB
�B
[B
=B
VB
 vB
�B
�B
�B
 \B
'�B
+kB
-]B
/�B
1[B
2aB
3hB
6�B
8lB
>B
@OB
@4B
AB
B�B
CGB
GzB
I�B
I�B
G�B
B[B
>�B
=VB
A B
@�B
?}B
=�B
;0B
:B
6B
3�B
9�B
?�B
@OB
?HB
>�B
=�B
;�B
<jB
;�B
;�B
:*B
9�B
8RB
7�B
7�B
:^B
>BB
=qB
<�B
>�B
@�B
B�B
C-B
BAB
BAB
BAB
CGB
C{B
C�B
B�B
AoB
AoB
AUB
?�B
?cB
=qB
;dB
9�B
3MB
3B
3�B
2�B
1�B
0;B
/OB
.}B
/ B
-�B
-�B
,=B
+�B
*�B
)DB
(�B
$�B
#B
 �B
 \B
�B
�B
�B
�B
yB
�B
�B
[B
TB
�B
�B
�B
TB
�B
�B
B
VB
�B
�B
NB
�B
�B
�B
�B
�B
�B
�B
xB
B
�B
�B
�B
0B
	B
�B
�B
�B
B
B
MB
B
�B
�B
DB
�B
�B
�B
4B
�B
HB
�B
\B
�B
"B
�B
B
�B
~B
JB
xB
B

#B
	�B
	B
�B
1B
�B
KB
EB
EB
B
�B
?B
�B
�B
�B
�B
SB
B
�B
GB
�B
�B
�B
{B
�B
[B
'B
�B
�B
oB
;B
oB
oB
UB
�B
oB
�B
�B
[B
�B
oB
�B
�B
�B
�B
'B
[B
'B
�B
�B
;B
B
 �B
;B
;B
;B
 B
B
 B
B
 �B
B
 B
B
B
B
 �B
 iB	��B	��B	��B	�}B	��B
 �B
 B
;B
 B
oB
oB
�B
 B
 iB	�}B	�cB	�}B	��B	��B
 4B
 �B
;B
�B
[B
AB
'B
�B
�B
�B
B
AB
�B
-B
�B
�B
�B
�B
�B
3B
�B
�B
�B
3B
�B
�B
�B
YB
%B
�B
zB
�B
1B
�B
	RB
	�B

#B

=B

XB

�B
^B
DB
�B
JB
B
jB
�B
�B
<B
�B
B
�B
�B
�B
�B
}B
�B
�B
�B
4B
hB
�B
�B
�B
�B
�B
�B
�B
�B
 B
 B
TB
oB
B
�B
&B
[B
FB
,B
�B
�B
2B
�B
MB
�B
�B
B
B
�B
�B
�B
�B
YB
yB
�B
1B
1B
B
B
eB
B
�B
�B
�B
�B
CB
CB
CB
B
B
)B
xB
�B
B
B
B
OB
B
�B
VB
�B
 'B
 'B
!|B
!HB
!-B
!bB
"4B
!�B
"�B
#nB
#�B
#�B
$�B
$�B
$�B
%FB
%�B
%�B
%�B
%�B
&�B
&�B
'8B
'�B
'�B
(XB
(�B
)*B
)*B
)�B
*B
*�B
*�B
*�B
*�B
*eB
,"B
,=B
,�B
,�B
-)B
-]B
-�B
-�B
-�B
-�B
.B
-�B
.}B
.�B
/OB
/iB
/�B
/�B
/�B
0oB
0�B
0�B
1[B
1vB
1�B
2-B
2|B
2�B
3hB
3MB
3�B
3�B
3�B
3�B
3�B
49B
4�B
4�B
4�B
4�B
4�B
5ZB
5�B
6B
6+B
6FB
6zB
7B
6�B
6�B
6�B
6�B
6�B
6�B
72B
7�B
7�B
7�B
7�B
7�B
8B
8RB
8�B
9$B
9	B
:B
:xB
:xB
:�B
;0B
;JB
;0B
;dB
;dB
;�B
<�B
<�B
=B
<�B
=VB
=�B
>B
>(B
>]B
>�B
>wB
>�B
?HB
?HB
?}B
?�B
?�B
?�B
@�B
A;B
AUB
A;B
A;B
A�B
B�B
B�B
B�B
B�B
CB
C{B
C�B
C�B
C�B
DgB
D�B
D�B
EmB
ESB
EmB
E�B
F?B
F�B
G_B
G�B
G�B
G�B
G�B
H�B
H�B
IB
I�B
J#B
J	B
I�B
I�B
JrB
J�B
J�B
KB
K^B
KDB
K)B
K�B
LB
LJB
LdB
L~B
L�B
L�B
L�B
L�B
L�B
L�B
M�B
M�B
N�B
N�B
N�B
O(B
O�B
O�B
PB
P.B
PHB
PHB
P}B
P}B
P�B
P�B
Q B
QB
Q�B
Q�B
RB
RB
R�B
S@B
S@B
SuB
S�B
S�B
S�B
S�B
T�B
U�B
U�B
U�B
VB
VSB
V�B
V�B
V�B
W
B
W�B
W�B
W�B
W�B
W�B
XB
X_B
X�B
X�B
X�B
YeB
YB
YB
Y�B
Y�B
ZB
ZB
ZB
ZQB
Z�B
[WB
[�B
[�B
[�B
\CB
\xB
\�B
]dB
]�B
^5B
^�B
^�B
_;B
_VB
_pB
_VB
_pB
_�B
_�B
_�B
`\B
`�B
`�B
`vB
`�B
`�B
`�B
`�B
aB
a-B
abB
a|B
a�B
a�B
b4B
bNB
b�B
b�B
b�B
c B
cTB
cnB
cTB
cTB
cnB
cB
b�B
bhB
b�B
b�B
b�B
c�B
c�B
c�B
c�B
c�B
d&B
d&B
dB
dB
d@B
d�B
d�B
d�B
eB
eB
ezB
e�B
ffB
ffB
f�B
f�B
gRB
gmB
h
B
hXB
hXB
h�B
h�B
i_B
i�B
i�B
i�B
jB
jKB
jKB
jKB
jB
j�B
j�B
k6B
k6B
kkB
k�B
k�B
l"B
l"B
l=B
l=B
lqB
l�B
l�B
l�B
l�B
mB
m)B
mwB
m�B
nIB
ncB
n}B
n}B
n}B
n�B
oB
o�B
p!B
p�B
p�B
p�B
p�B
p�B
q�B
q�B
q�B
rB
rB
rB
r�B
r�B
r�B
s3B
sB
shB
s�B
s�B
tB
tB
tTB
t�B
t�B
t�B
t�B
t�B
t�B
u%B
u?B
uZB
utB
u�B
u�B
u�B
vB
v`B
v`B
vFB
v+B
v�B
v�B
v�B
wB
v�B
wfB
w�B
w�B
xB
xB
x8B
xRB
xB
x8B
xlB
x�B
x�B
x�B
yXB
yrB
y�B
y�B
y�B
z*B
z^B
zxB
z�B
z�B
z�B
z�B
z�B
z�B
z�B
z�B
{0B
{JB
{B
{�B
{�B
|B
|B
|B
|B
|PB
|PB
|jB
|�B
|�B
|�B
|�B
|�B
}B
}B
}VB
}�B
}qB
}qB
}qB
}�B
~(B
~]B
~wB
~wB
~�B
B
.B
cB
�B
�B
�B
�B
�B
��B
��B
��B
�B
�;B
��B
��B
�B
�AB
�AB
��B
��B
��B
��B
�B
�-B
�GB
�aB
�{B
�aB
�aB
�{B
��B
��B
�B
�B
�3B
�3B
�3B
��B
�9B
�mB
�mB
��B
�mB
�B
�SB
��B
��111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                            JA  ARFMdecpA30a                                                                20220604105246  IP                  G�O�G�O�G�O�                JA  ARFMfmtp2.8                                                                 20220604192610  IP                  G�O�G�O�G�O�                JA  ARCAsspa3.1b                                                                20220604192610  IP  PRES            G�O�G�O�G�O�                JA      jafc1.0                                                                 20220604192610                      G�O�G�O�G�O�                JA  ARGQrqcpc3.6                                                                20220605042618  QCP$                G�O�G�O�G�O�         20DF37EJA  ARGQrqcpc3.6                                                                20220605042618  QCF$                G�O�G�O�G�O�               0JA  ARUP                                                                        20220610161505                      G�O�G�O�G�O�                