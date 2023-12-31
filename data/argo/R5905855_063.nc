CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       JMA    source        
Argo float     history       E2022-06-04T19:21:43Z creation;2022-06-04T19:21:45Z conversion to V3.1      
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
_FillValue                    �Argo profile    3.1 1.2 19500101000000  20220604192143  20220610151509  5905855                                                                 JAMSTEC                                                         PRES            TEMP            PSAL               ?A   JA                                  2B  A   APEX                            8423                            2.11.2                          846 @�C����1   @�C�=��@-��z�H�clbM��1   GPS     A   A   A   Primary sampling: averaged [2 dbar bin average for 1 Hz CTD]                                                                                                                                                                                                       @,��@�  @�  A   A   A@  A`  A�  A�  A�  A�  A�  A���A�ffA�  A�33B  BffB33B!��B&��B0  B8  B@  BF��BP  BU��B`  BhffBo��BxffB���B�  B�  B�  B���B�  B�  B�  B�  B�  B�  B�33B�33B�ffB�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�ffB���B�  B�  B�33B�  B���B���C   C  C  C  C  C
  C  C  C  C  C�C  C  C  C  C  C   C"  C$  C&  C(  C*�C,�C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dt� Du  Du� Dv  Dv� Dw  Dw� Dx  Dx� Dy  Dy� Dz  Dz� D{  D{� D|  D|� D}  D}� D~  D~� D  D� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�|�D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D��3D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D D�� D�  D�@ DÀ D�� D�  D�@ DĀ D�� D�  D�@ Dŀ D�� D�  D�C3Dƀ D�� D�  D�@ Dǀ D�� D�  D�@ DȀ D�� D�  D�@ Dɀ D�� D�  D�@ Dʀ D�� D�  D�@ Dˀ D�� D�  D�@ D̀ D�� D�  D�@ D̀ D�� D�  D�@ D΀ D�� D�  D�@ Dπ D�� D�  D�@ DЀ D�� D�  D�@ Dр D�� D�  D�@ DҀ D�� D�  D�@ DӀ D�� D�  D�@ DԀ D�� D�  D�@ DՀ D�� D�  D�@ Dր D�� D�  D�@ D׀ D�� D�  D�@ D؀ D�� D�  D�@ Dـ D�� D�  D�@ Dڀ D�� D�  D�@ Dۀ D�� D�  D�@ D܀ D�� D�  D�@ D݀ D�� D�  D�@ Dހ D�� D�  D�@ D߀ D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D���D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D��3111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111@+�@~�R@�\)@�\)A�A?�A_�A�A��
A��
A��
A��
AУ�A�=pA��
A�
=B�BQ�B�B!�B&�RB/�B7�B?�BF�RBO�BU�B_�BhQ�Bo�BxQ�B��]B���B���B���B�B���B���B���B���B���B���B�(�B�(�B�\)B���B���B���B���B���B���B���B���B���B���B�\)B�B���B���B�(�B���B�B�B���C��C��C��C��C	��C��C��C��C��C{C��C��C��C��C��C��C!��C#��C%��C'��C*{C,{C-��C/��C1��C3��C5��C7��C9��C;��C=��C?��CA��CC��CE��CG��CI��CK��CM��CO��CQ��CS��CU��CW��CY��C[��C]��C_��Ca��Cc��Ce��Cg��Ci��Ck��Cm��Co��Cq��Cs��Cu��Cw��Cy��C{��C}��C��C��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC�
>C��qC��qC��qC��qC��qC��qC�
>C��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC�
>C��qC��qC��qC��qC��qC��qC��qC��qC��qC��qD ~�D ��D~�D��D~�D��D~�D��D~�D��D~�D��D~�D��D~�D��D~�D��D	~�D	��D
~�D
��D~�D��D~�D��D~�D��D~�D��D~�D��D~�D��D~�D��D~�D��D~�D��D~�D��D~�D��D~�D��D~�D��D~�D��D~�D��D~�D��D~�D��D~�D��D~�D��D~�D��D~�D��D ~�D ��D!~�D!��D"~�D"��D#~�D#��D$~�D$��D%~�D%��D&~�D&��D'~�D'��D(~�D(��D)~�D)��D*~�D*��D+~�D+��D,~�D,��D-~�D-��D.~�D.��D/~�D/��D0~�D0��D1~�D1��D2~�D2��D3~�D3��D4~�D4��D5~�D5��D6~�D6��D7~�D7��D8~�D8��D9~�D9��D:~�D:��D;~�D;��D<~�D<��D=~�D=��D>~�D>��D?~�D?��D@~�D@��DA~�DA��DB~�DB��DC~�DC��DD~�DD��DE~�DE��DF~�DF��DG~�DG��DH~�DH��DI~�DI��DJ~�DJ��DK~�DK��DL~�DL��DM~�DM��DN~�DN��DO~�DO��DP~�DP��DQ~�DQ��DR~�DR��DS~�DS��DT~�DT��DU~�DU��DV~�DV��DW~�DW��DX~�DX��DY~�DY��DZ~�DZ��D[~�D[��D\~�D\��D]~�D]��D^~�D^��D_~�D_��D`~�D`��Da~�Da��Db~�Db��Dc~�Dc��Dd~�Dd��De~�De��Df~�Df��Dg~�Dg��Dh~�Dh��Di~�Di��Dj~�Dj��Dk~�Dk��Dl~�Dl��Dm~�Dm��Dn~�Dn��Do~�Do��Dp~�Dp��Dq~�Dq��Dr~�Dr��Ds~�Ds��Dt~�Dt��Du~�Du��Dv~�Dv��Dw~�Dw��Dx~�Dx��Dy~�Dy��Dz~�Dz��D{~�D{��D|~�D|��D}~�D}��D~~�D~��D~�D��D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�|)D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D���D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D¿\D��\D�?\D�\Dÿ\D��\D�?\D�\DĿ\D��\D�?\D�\Dſ\D��\D�B�D�\Dƿ\D��\D�?\D�\Dǿ\D��\D�?\D�\Dȿ\D��\D�?\D�\Dɿ\D��\D�?\D�\Dʿ\D��\D�?\D�\D˿\D��\D�?\D�\D̿\D��\D�?\D�\DͿ\D��\D�?\D�\Dο\D��\D�?\D�\DϿ\D��\D�?\D�\Dп\D��\D�?\D�\Dѿ\D��\D�?\D�\Dҿ\D��\D�?\D�\Dӿ\D��\D�?\D�\DԿ\D��\D�?\D�\Dտ\D��\D�?\D�\Dֿ\D��\D�?\D�\D׿\D��\D�?\D�\Dؿ\D��\D�?\D�\Dٿ\D��\D�?\D�\Dڿ\D��\D�?\D�\Dۿ\D��\D�?\D�\Dܿ\D��\D�?\D�\Dݿ\D��\D�?\D�\D޿\D��\D�?\D�\D߿\D��\D�?\D�\D�\D��\D�?\D�\D�\D��\D�?\D�\D�\D��\D�?\D�\D�\D��\D�?\D�\D�\D��\D�?\D�\D�\D��\D�?\D�\D�\D��\D�?\D�\D�\D��\D�?\D�\D�\D��\D�?\D�\D�\D��\D�?\D�\D�\D��\D�?\D�\D�\D��\D�?\D�\D�\D��\D�?\D�\D��\D��\D�?\D�\D�\D��\D�?\D�\D�\D��\D�?\D�\D�\D��\D�?\D�\D�\D��\D�?\D�\D�\D��\D�?\D�\D�\D��\D�?\D�\D��\D��\D�?\D�\D��)D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111A�L0A�~]A�uAޥFAޅ�A�UgA�	A��A��	A��A��8A���A�уA��dAݨ$A݂�Aݍ�Aݯ�Aݟ�A�qvAܜxAؼ�A��fA֐bA�p�A��A˴�A���A��A���A�7A��bA�e`A�}"A��vA�eA��fA��DA�[#A�A�0�A�a�A�A��A�2�A�*�A���A�!bA�WsA��wA��MA��A}��A{\�At�UAp�rAo:�Aly>Ai��Ae��A`�AY�ARAMX�AG�1AC�AA�AAm�A>�IA;�9A;X�A;�A:ߤA:�AA9��A8��A8��A85�A7��A7��A7@OA5��A3�A0�gA-��A,��A,&�A+(�A)A)��A(�RA(��A(n�A'h�A()_A(�A'��A'+kA%ϫA#8�A!�A��A+A��A�~AIRAn�A��AA�AoA��A��AdZA�AߤA�A�AOvA��A'RA~AQ�A�A��AzA�TAv`A�A��A=qA�.ADgA0UA��A�A=qA�AA�tA�A�	Ae�A�KA�9A-A
�MA
�"A
��A
�A	y>A	�AC�A�A+A��A�[A��A��A͟A�vAS�A�XAs�AɆA��A�}A�.An/AA A ѷA �@�S&@�-w@���@�&�@��k@��@��@��@���@���@�oi@�
�@��5@�~@���@�^�@�1�@��@�~(@�n�@�p;@��@��@���@�K�@�@��@�G@�|�@��@�;�@�|�@��E@��@�@O@��]@�	�@�)_@�L0@��|@䩓@�Y@�_@��Z@�#�@��@�ی@��s@⟾@��@�Ov@���@��@�@��/@�{�@��.@��@ߩ�@�+�@��@ނA@�2�@�zx@��v@�bN@�}�@ږ�@�#:@��@٫�@�`B@��K@�E�@��K@�u�@�,�@�;@���@�҉@�|�@� �@���@�t�@��@Ԛ@�Ft@�)�@��Q@�Mj@�4n@іS@��@Ј�@�PH@��A@�c�@Ξ@�!@��Q@͡�@͊�@�qv@�Mj@�9�@�	l@�ی@̘_@�($@˟V@��P@ʼj@�Ft@�b@�خ@�\)@��X@Ț@ș1@Ȉ�@�M@��K@�rG@�(�@�j@��@ŖS@�@O@�YK@���@èX@�Z�@�Ɇ@�@�w�@��@��t@��F@�}�@���@�(�@�,=@��Z@���@��@�"h@�{J@�A�@�֡@�6�@��@�X@��@���@�@��C@��P@�U2@�1'@�~@���@��@�#:@���@�+@��]@�e�@��#@���@�zx@�=�@���@�w�@�N�@��4@�'�@��K@���@��6@��z@��z@���@�y>@�Z@��e@��@�s�@�W�@��@�l�@��@���@��u@��f@�RT@���@��Z@��T@��&@��@�Z@�=q@��n@�g�@�E9@���@���@��@�	l@��@�-@��@�v`@�V@��@���@���@���@�?�@��@��W@���@�K�@�ں@���@���@�]d@��@��$@�;@��R@���@�-�@��W@���@�x@�j�@�,�@��E@��.@�bN@�	@�s�@��8@���@��F@���@��+@�h�@�K^@��@�{@��@��@��m@���@�\)@�7L@�,�@��@���@�d�@�7�@��@�(@��_@�j@�I�@�{@��@���@�8@��/@���@�_@�/�@��@�� @��@��~@�/@�)_@�/@�V@��]@��@�{�@�.�@��@���@�rG@�RT@�5�@��@���@��@�N<@��U@�N�@���@�y�@���@���@��!@�z�@�@���@��4@��5@��@�A�@��)@�s�@�X@�� @�($@��>@��n@��~@�RT@��?@�_@�4n@�V�@�@�@���@���@��*@���@�zx@�ی@��@���@�~(@�R�@��@���@�K�@���@���@��@���@�o @�'�@��@���@��1@�u%@�Ov@�?�@�:*@��@�p�@�&@��5@���@�*�@��j@���@�n/@�O@�"�@��[@���@�v�@�q@�[�@�1'@�@��@Y@~.�@}x�@}&�@|��@|q@|M@{��@{a@{�@z��@z5?@y��@y#�@x��@xg8@w�@w��@wb�@w�@vO@u��@u��@uS&@u�@t�?@ty>@tM@sخ@s��@s�@rߤ@r�@rB[@r�@qm]@p�`@pz�@pFt@p	�@o�w@og�@o=@n�]@n�B@n�@m��@mm]@mO�@mq@lg8@k�m@k��@k�k@kW?@k�@j��@j��@j8�@ik�@h�p@hr�@hU2@g�&@g�P@g'�@fߤ@f��@f$�@fu@e�D@e�j@e��@eY�@d�K@d�@d>B@c��@c$t@b�}@bq�@bTa@b5?@a�@au�@a�@`�Y@`G@_��@_b�@^�'@^a|@^#:@]@]X@\�[@\Xy@\�@[P�@Z��@Z��@Z�@Z.�@Y�t@YX@Y�@X�@XA�@WJ#@V��@V6�@U�D@U`B@UV@TM@T�@S��@S33@R�8@R�@RYK@R�@Q�7@P�@P�z@PA�@Ox@N��@N�@M�T@M}�@M�@L��@L�@LQ�@K�@Kخ@Kl�@J�@J�}@J1�@I��@Ip�@I/@HtT@G�{@F�B@F8�@E��@EV@D�z@DK^@Db@C�q@Cqv@B�M@B($@A�N@A��@A�=@Ac@AA @@�/@@�4@@m�@@PH@@7@?ݘ@?�@?qv@?,�@>��@>	@=�9@=��@=S&@=�@<�[@<�Y@<H@;�@;��@;;d@:��@:��@:;�@9�9@9w2@9&�@8�@8��@8��@8I�@7�@7��@7x@7_p@7�@6�s@6�b@6Q@66�@5�j@5��@5q@4�`@4�D@4_@49X@3�;@3{J@2��@2�]@2��@2GE@1��@1��@1@@0��@0��@0�o@0C-@/�@/�m@/�f@/K�@/S@.xl@._@-��@-�X@-T�@-�@,ی@,֡@,�$@,j@+�@+��@+��@+��@+RT@+;d@+ i@*{�@*c @*@)��@)+�@(��@(�z@(S�@'��@'��@'g�@'H�@'�@&��@&\�@%��@%�'@%#�@$�D@$1'@#�&@#��@#a@#�@"��@"h
@"V@";�@")�@!�>@!��@!|@!8�@!�@ �@ �e@ Xy@ (�@ 1@�@�@��@j�@4�@(@��@p;@4@�T@��@��@L�@��@�@�p@��@�j@w�@K^@~@�A@��@��@�@��@n/@P�@C�@;d@33@$t@@�B@�!@�@�@�@��@~�@=q@��@��@��@p�@hs@[W@:�@�@�@��@�|@�K@��@��@�u@_@M@4n@�@�@�;@�[@v`@>�@)_@�@҉@�L@ff@�@�#@��@p�@�@�@֡@�U@�@C-@��@�F@�*@��@y�@K�@�2@ȴ@��@��@c @@�@.�@$�@@�>@��@�7@f�@�@�)@�@�o@[�@PH@A�@	�@��@��@��@��@{J@a@Mj@4�@�@��@s�@_@�^@�h@Y�@ \@V@��@��@��@�u@h�@K^@<�@@ݘ@��@�F@��@~�@x@P�@�@
�@
ߤ@
��@
u%@
R�@
#:@	�D@	��@	��@	`B@	/@��@�@��@��@q@V�@H@9X@-�@1@�}@�f@�{@�{@v`@J#@@�,@xl@H�@.�@O@	@ �@�o@��@o @G�@<6@(�@V@@��@�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111A�L0A�~]A�uAޥFAޅ�A�UgA�	A��A��	A��A��8A���A�уA��dAݨ$A݂�Aݍ�Aݯ�Aݟ�A�qvAܜxAؼ�A��fA֐bA�p�A��A˴�A���A��A���A�7A��bA�e`A�}"A��vA�eA��fA��DA�[#A�A�0�A�a�A�A��A�2�A�*�A���A�!bA�WsA��wA��MA��A}��A{\�At�UAp�rAo:�Aly>Ai��Ae��A`�AY�ARAMX�AG�1AC�AA�AAm�A>�IA;�9A;X�A;�A:ߤA:�AA9��A8��A8��A85�A7��A7��A7@OA5��A3�A0�gA-��A,��A,&�A+(�A)A)��A(�RA(��A(n�A'h�A()_A(�A'��A'+kA%ϫA#8�A!�A��A+A��A�~AIRAn�A��AA�AoA��A��AdZA�AߤA�A�AOvA��A'RA~AQ�A�A��AzA�TAv`A�A��A=qA�.ADgA0UA��A�A=qA�AA�tA�A�	Ae�A�KA�9A-A
�MA
�"A
��A
�A	y>A	�AC�A�A+A��A�[A��A��A͟A�vAS�A�XAs�AɆA��A�}A�.An/AA A ѷA �@�S&@�-w@���@�&�@��k@��@��@��@���@���@�oi@�
�@��5@�~@���@�^�@�1�@��@�~(@�n�@�p;@��@��@���@�K�@�@��@�G@�|�@��@�;�@�|�@��E@��@�@O@��]@�	�@�)_@�L0@��|@䩓@�Y@�_@��Z@�#�@��@�ی@��s@⟾@��@�Ov@���@��@�@��/@�{�@��.@��@ߩ�@�+�@��@ނA@�2�@�zx@��v@�bN@�}�@ږ�@�#:@��@٫�@�`B@��K@�E�@��K@�u�@�,�@�;@���@�҉@�|�@� �@���@�t�@��@Ԛ@�Ft@�)�@��Q@�Mj@�4n@іS@��@Ј�@�PH@��A@�c�@Ξ@�!@��Q@͡�@͊�@�qv@�Mj@�9�@�	l@�ی@̘_@�($@˟V@��P@ʼj@�Ft@�b@�خ@�\)@��X@Ț@ș1@Ȉ�@�M@��K@�rG@�(�@�j@��@ŖS@�@O@�YK@���@èX@�Z�@�Ɇ@�@�w�@��@��t@��F@�}�@���@�(�@�,=@��Z@���@��@�"h@�{J@�A�@�֡@�6�@��@�X@��@���@�@��C@��P@�U2@�1'@�~@���@��@�#:@���@�+@��]@�e�@��#@���@�zx@�=�@���@�w�@�N�@��4@�'�@��K@���@��6@��z@��z@���@�y>@�Z@��e@��@�s�@�W�@��@�l�@��@���@��u@��f@�RT@���@��Z@��T@��&@��@�Z@�=q@��n@�g�@�E9@���@���@��@�	l@��@�-@��@�v`@�V@��@���@���@���@�?�@��@��W@���@�K�@�ں@���@���@�]d@��@��$@�;@��R@���@�-�@��W@���@�x@�j�@�,�@��E@��.@�bN@�	@�s�@��8@���@��F@���@��+@�h�@�K^@��@�{@��@��@��m@���@�\)@�7L@�,�@��@���@�d�@�7�@��@�(@��_@�j@�I�@�{@��@���@�8@��/@���@�_@�/�@��@�� @��@��~@�/@�)_@�/@�V@��]@��@�{�@�.�@��@���@�rG@�RT@�5�@��@���@��@�N<@��U@�N�@���@�y�@���@���@��!@�z�@�@���@��4@��5@��@�A�@��)@�s�@�X@�� @�($@��>@��n@��~@�RT@��?@�_@�4n@�V�@�@�@���@���@��*@���@�zx@�ی@��@���@�~(@�R�@��@���@�K�@���@���@��@���@�o @�'�@��@���@��1@�u%@�Ov@�?�@�:*@��@�p�@�&@��5@���@�*�@��j@���@�n/@�O@�"�@��[@���@�v�@�q@�[�@�1'@�@��@Y@~.�@}x�@}&�@|��@|q@|M@{��@{a@{�@z��@z5?@y��@y#�@x��@xg8@w�@w��@wb�@w�@vO@u��@u��@uS&@u�@t�?@ty>@tM@sخ@s��@s�@rߤ@r�@rB[@r�@qm]@p�`@pz�@pFt@p	�@o�w@og�@o=@n�]@n�B@n�@m��@mm]@mO�@mq@lg8@k�m@k��@k�k@kW?@k�@j��@j��@j8�@ik�@h�p@hr�@hU2@g�&@g�P@g'�@fߤ@f��@f$�@fu@e�D@e�j@e��@eY�@d�K@d�@d>B@c��@c$t@b�}@bq�@bTa@b5?@a�@au�@a�@`�Y@`G@_��@_b�@^�'@^a|@^#:@]@]X@\�[@\Xy@\�@[P�@Z��@Z��@Z�@Z.�@Y�t@YX@Y�@X�@XA�@WJ#@V��@V6�@U�D@U`B@UV@TM@T�@S��@S33@R�8@R�@RYK@R�@Q�7@P�@P�z@PA�@Ox@N��@N�@M�T@M}�@M�@L��@L�@LQ�@K�@Kخ@Kl�@J�@J�}@J1�@I��@Ip�@I/@HtT@G�{@F�B@F8�@E��@EV@D�z@DK^@Db@C�q@Cqv@B�M@B($@A�N@A��@A�=@Ac@AA @@�/@@�4@@m�@@PH@@7@?ݘ@?�@?qv@?,�@>��@>	@=�9@=��@=S&@=�@<�[@<�Y@<H@;�@;��@;;d@:��@:��@:;�@9�9@9w2@9&�@8�@8��@8��@8I�@7�@7��@7x@7_p@7�@6�s@6�b@6Q@66�@5�j@5��@5q@4�`@4�D@4_@49X@3�;@3{J@2��@2�]@2��@2GE@1��@1��@1@@0��@0��@0�o@0C-@/�@/�m@/�f@/K�@/S@.xl@._@-��@-�X@-T�@-�@,ی@,֡@,�$@,j@+�@+��@+��@+��@+RT@+;d@+ i@*{�@*c @*@)��@)+�@(��@(�z@(S�@'��@'��@'g�@'H�@'�@&��@&\�@%��@%�'@%#�@$�D@$1'@#�&@#��@#a@#�@"��@"h
@"V@";�@")�@!�>@!��@!|@!8�@!�@ �@ �e@ Xy@ (�@ 1@�@�@��@j�@4�@(@��@p;@4@�T@��@��@L�@��@�@�p@��@�j@w�@K^@~@�A@��@��@�@��@n/@P�@C�@;d@33@$t@@�B@�!@�@�@�@��@~�@=q@��@��@��@p�@hs@[W@:�@�@�@��@�|@�K@��@��@�u@_@M@4n@�@�@�;@�[@v`@>�@)_@�@҉@�L@ff@�@�#@��@p�@�@�@֡@�U@�@C-@��@�F@�*@��@y�@K�@�2@ȴ@��@��@c @@�@.�@$�@@�>@��@�7@f�@�@�)@�@�o@[�@PH@A�@	�@��@��@��@��@{J@a@Mj@4�@�@��@s�@_@�^@�h@Y�@ \@V@��@��@��@�u@h�@K^@<�@@ݘ@��@�F@��@~�@x@P�@�@
�@
ߤ@
��@
u%@
R�@
#:@	�D@	��@	��@	`B@	/@��@�@��@��@q@V�@H@9X@-�@1@�}@�f@�{@�{@v`@J#@@�,@xl@H�@.�@O@	@ �@�o@��@o @G�@<6@(�@V@@��@�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111B{JBm�Bd�Bd�BjBh>B_pB`vB`�Bc�B`�B_�BaBg�B`�BbBgBu?BwfBsB��B�iB�B��B�^B	=�B	>wB	��B	�B	�xB	ϑB
[�B
��B
�WB
�}B
��B
�B
�8B
� B
r�B
_pB
UMB
YB
`�B
_�B
>�B
9�B
'B
EB	�B	уB	��B	��B	��B	�3B	m�B	g�B	\B	RoB	@B	$tB	EB�B�{B��B�B��B��B� B�HB�(BοB�bB��B�@B�nB�B��B	�B	�B	jB	9B	�B�2B��B�>B��B�MB�B	0B	 B	/�B	<B	3�B	RB	pUB	x�B	� B	�9B	��B	vzB	i�B	f�B	p�B	w�B	t�B	p�B	m�B	z�B	�B	�B	�[B	��B	��B	��B	�B	�.B	�(B	��B	��B	��B	��B	�#B	��B	��B	�wB	��B	��B	��B	�[B	�4B	nB	l�B	mwB	n/B	m�B	o�B	�+B	��B	�]B	��B	��B	�bB	�OB	��B	��B	�B	�'B	��B	�kB	�KB	��B	�:B	��B	�,B	�B	�B	��B	��B	�B	��B	��B	�B	�-B	�B	��B	�B	�MB	��B	�TB	�9B	�B	��B	��B	�cB	��B	B	�9B	ȀB	ȀB	�B	��B	�KB	�fB	ɺB	�<B	�(B	�\B	ЗB	҉B	�B	�vB	�{B	��B	�MB	�&B	�B	�B	��B	ۦB	��B	��B	��B	�~B	��B	�~B	ޞB	��B	�B	�B	�\B	��B	�|B	�hB	�B	�B	��B	�ZB	�@B	�@B	��B	�tB	�B	�,B	��B	��B	�,B	��B	�tB	�tB	�nB	�nB	�nB	�tB	�B	�zB	�2B	��B	�`B	�B	��B	�B	�FB	�FB	�,B	�`B	�B	�B	�zB	��B	��B	�B	�B	�8B	�RB	�B	�8B	��B	��B	�DB	�B	�B	�QB	��B	�B	��B	��B	��B	�B	��B	��B	��B	��B	��B	��B	�B	��B	�cB	�cB	�iB	�B	�B	�B	�B	�B	��B	�GB	�GB	��B	��B	�-B	�-B	��B	��B	��B	�B	�GB	�B	��B	�B	��B	��B	�nB	��B	�B	�aB	��B	�B	�B	�B	�B	�+B	��B	�`B	��B	��B	��B	��B	�`B	��B	��B	��B	��B	��B	��B	�8B	��B	��B	�LB	�2B	��B	��B	�fB	�fB	��B	��B	��B	�	B	�B	�B	��B	��B	�0B	��B	��B	�dB	��B	�PB	��B	��B	�}B
B
uB
�B
�B
�B
�B
�B
�B
EB
_B
�B
_B
�B
gB
�B
�B
GB
3B
�B
oB
�B
B
�B
�B
�B
�B
�B
�B
_B
B
�B
KB
fB
	B
	�B

XB
�B
�B
VB
.B
�B
�B
�B
B
.B
HB
�B
hB
B
4B
�B
B
B
NB
4B
�B
�B
�B
NB
�B
�B
�B
HB
�B
�B
NB
�B
�B
�B
oB
�B
�B
B
2B
gB
�B
�B
�B
B
B
�B
�B
�B
�B
&B
@B
&B
�B
,B
FB
�B
MB
�B
�B
�B
mB
�B
mB
�B
�B
�B
�B
�B
�B
�B
B
/B
�B
�B
�B
5B
OB
5B
B
!B
�B
 'B
 B
!B
�B
jB
�B
 �B
 vB
 �B
!B
 �B
!�B
"�B
"�B
"�B
"�B
#�B
#�B
"�B
"�B
#TB
#TB
"�B
"NB
!�B
"NB
#�B
%�B
%�B
&B
%�B
%FB
$�B
$�B
%�B
&�B
'�B
(>B
)B
(�B
)�B
*B
*0B
+B
+�B
,"B
,�B
,�B
-B
-)B
-)B
-�B
-�B
-�B
.}B
/�B
0;B
0UB
0UB
1B
1[B
2�B
33B
3MB
3�B
3hB
3B
3MB
3�B
4TB
4�B
4nB
4nB
4�B
5tB
5?B
4�B
5%B
5?B
5tB
5�B
5�B
6FB
6�B
6�B
7B
7fB
7�B
7fB
7fB
72B
7B
8B
8�B
8�B
9	B
9>B
9XB
9rB
9�B
9�B
9�B
:B
:^B
:DB
:xB
:�B
:DB
:�B
;B
;B
:�B
:�B
;B
;dB
;dB
;�B
<B
;�B
;�B
<B
<�B
=B
=�B
=�B
=�B
=�B
=�B
>B
>B
=�B
>BB
>�B
>�B
>�B
>�B
?B
?B
?.B
?HB
?�B
@B
@ B
@ B
@4B
@ B
@OB
@iB
@�B
@�B
A;B
A B
A�B
A�B
AoB
A�B
A�B
A�B
B�B
CB
DB
D3B
DMB
D�B
D�B
D�B
EB
E9B
E�B
E�B
E�B
F?B
FtB
F?B
FYB
F�B
F�B
GEB
G_B
GzB
G�B
H�B
H�B
I7B
I7B
I�B
I�B
JrB
J�B
J�B
KxB
K^B
K�B
K�B
K�B
LB
LdB
LdB
LdB
L0B
LdB
MB
M6B
M6B
M�B
M�B
N"B
NpB
N�B
N�B
OB
O\B
O\B
O�B
O�B
PHB
PbB
P�B
Q�B
R:B
R�B
SB
S�B
T,B
TFB
T{B
T�B
T�B
UB
U�B
U�B
U�B
VB
VB
VB
VmB
V�B
V�B
V�B
V�B
W
B
W
B
W?B
W?B
W�B
XEB
XEB
XyB
XyB
X�B
X�B
Y1B
YeB
Y�B
YB
Z7B
ZkB
Z�B
Z�B
[=B
[�B
[�B
[�B
[�B
[�B
\)B
\�B
\�B
\�B
\�B
]B
]~B
]�B
]�B
]�B
^5B
^5B
^jB
^�B
^�B
^�B
_!B
_VB
_�B
`B
`B
`'B
`\B
`�B
a-B
a-B
abB
a�B
a|B
a�B
a�B
a�B
bhB
b�B
b�B
cTB
cnB
cnB
c�B
d&B
d@B
dZB
dZB
dZB
d�B
eB
d�B
eB
eB
e`B
e,B
e`B
e�B
e�B
f2B
ffB
f�B
f�B
gB
gRB
g�B
h
B
h
B
h$B
hXB
h�B
h�B
h�B
iDB
i�B
jB
jeB
j�B
j�B
j�B
kQB
kQB
k�B
k�B
k�B
k�B
lB
lWB
lWB
l�B
l�B
l�B
mB
m]B
mwB
mwB
m�B
m�B
m�B
m�B
n/B
nB
n}B
n�B
o B
o5B
o5B
o5B
o�B
o�B
o�B
o�B
pB
o�B
pB
p;B
poB
p�B
p�B
p�B
p�B
p�B
qB
qB
q'B
qAB
qAB
q'B
qAB
qvB
qvB
qvB
qvB
qvB
qvB
q�B
q�B
q�B
r-B
r|B
raB
r|B
r|B
r�B
r�B
r�B
r�B
r�B
r�B
s3B
s3B
sMB
s�B
s�B
s�B
s�B
s�B
s�B
s�B
tB
tnB
t9B
tnB
t�B
t�B
uB
u?B
uZB
utB
u�B
v+B
vzB
v�B
v�B
vzB
v�B
w2B
w�B
w�B
w�B
w�B
w�B
xRB
xlB
xlB
x�B
x�B
x�B
x�B
yrB
y�B
y�B
y�B
z*B
z*B
zxB
z�B
z�B
{B
{JB
{0B
{JB
{B
{�B
{�B
{�B
|B
|B
|PB
|6B
|jB
|�B
|�B
}"B
}�B
}�B
~B
~BB
~wB
~�B
~wB
~�B
~�B
~�B
B
B
B
cB
}B
�B
�B
� B
�B
�B
�OB
�iB
��B
��B
��B
�B
�;B
�;B
�oB
��B
��B
��B
�'B
��B
��B
��B
��B
�-B
�B
�GB
�GB
�aB
�{B
��B
��B
��B
��B
��B
�B
�MB
��B
��B
�B
�9B
�9B
�B
�9B
�9B
��B
��B
��B
�B
�%B
�%B
�YB
�?B
�t111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111B{JBm�Bd�Bd�BjBh>B_pB`vB`�Bc�B`�B_�BaBg�B`�BbBgBu?BwfBsB��B�iB�B��B�^B	=�B	>wB	��B	�B	�xB	ϑB
[�B
��B
�WB
�}B
��B
�B
�8B
� B
r�B
_pB
UMB
YB
`�B
_�B
>�B
9�B
'B
EB	�B	уB	��B	��B	��B	�3B	m�B	g�B	\B	RoB	@B	$tB	EB�B�{B��B�B��B��B� B�HB�(BοB�bB��B�@B�nB�B��B	�B	�B	jB	9B	�B�2B��B�>B��B�MB�B	0B	 B	/�B	<B	3�B	RB	pUB	x�B	� B	�9B	��B	vzB	i�B	f�B	p�B	w�B	t�B	p�B	m�B	z�B	�B	�B	�[B	��B	��B	��B	�B	�.B	�(B	��B	��B	��B	��B	�#B	��B	��B	�wB	��B	��B	��B	�[B	�4B	nB	l�B	mwB	n/B	m�B	o�B	�+B	��B	�]B	��B	��B	�bB	�OB	��B	��B	�B	�'B	��B	�kB	�KB	��B	�:B	��B	�,B	�B	�B	��B	��B	�B	��B	��B	�B	�-B	�B	��B	�B	�MB	��B	�TB	�9B	�B	��B	��B	�cB	��B	B	�9B	ȀB	ȀB	�B	��B	�KB	�fB	ɺB	�<B	�(B	�\B	ЗB	҉B	�B	�vB	�{B	��B	�MB	�&B	�B	�B	��B	ۦB	��B	��B	��B	�~B	��B	�~B	ޞB	��B	�B	�B	�\B	��B	�|B	�hB	�B	�B	��B	�ZB	�@B	�@B	��B	�tB	�B	�,B	��B	��B	�,B	��B	�tB	�tB	�nB	�nB	�nB	�tB	�B	�zB	�2B	��B	�`B	�B	��B	�B	�FB	�FB	�,B	�`B	�B	�B	�zB	��B	��B	�B	�B	�8B	�RB	�B	�8B	��B	��B	�DB	�B	�B	�QB	��B	�B	��B	��B	��B	�B	��B	��B	��B	��B	��B	��B	�B	��B	�cB	�cB	�iB	�B	�B	�B	�B	�B	��B	�GB	�GB	��B	��B	�-B	�-B	��B	��B	��B	�B	�GB	�B	��B	�B	��B	��B	�nB	��B	�B	�aB	��B	�B	�B	�B	�B	�+B	��B	�`B	��B	��B	��B	��B	�`B	��B	��B	��B	��B	��B	��B	�8B	��B	��B	�LB	�2B	��B	��B	�fB	�fB	��B	��B	��B	�	B	�B	�B	��B	��B	�0B	��B	��B	�dB	��B	�PB	��B	��B	�}B
B
uB
�B
�B
�B
�B
�B
�B
EB
_B
�B
_B
�B
gB
�B
�B
GB
3B
�B
oB
�B
B
�B
�B
�B
�B
�B
�B
_B
B
�B
KB
fB
	B
	�B

XB
�B
�B
VB
.B
�B
�B
�B
B
.B
HB
�B
hB
B
4B
�B
B
B
NB
4B
�B
�B
�B
NB
�B
�B
�B
HB
�B
�B
NB
�B
�B
�B
oB
�B
�B
B
2B
gB
�B
�B
�B
B
B
�B
�B
�B
�B
&B
@B
&B
�B
,B
FB
�B
MB
�B
�B
�B
mB
�B
mB
�B
�B
�B
�B
�B
�B
�B
B
/B
�B
�B
�B
5B
OB
5B
B
!B
�B
 'B
 B
!B
�B
jB
�B
 �B
 vB
 �B
!B
 �B
!�B
"�B
"�B
"�B
"�B
#�B
#�B
"�B
"�B
#TB
#TB
"�B
"NB
!�B
"NB
#�B
%�B
%�B
&B
%�B
%FB
$�B
$�B
%�B
&�B
'�B
(>B
)B
(�B
)�B
*B
*0B
+B
+�B
,"B
,�B
,�B
-B
-)B
-)B
-�B
-�B
-�B
.}B
/�B
0;B
0UB
0UB
1B
1[B
2�B
33B
3MB
3�B
3hB
3B
3MB
3�B
4TB
4�B
4nB
4nB
4�B
5tB
5?B
4�B
5%B
5?B
5tB
5�B
5�B
6FB
6�B
6�B
7B
7fB
7�B
7fB
7fB
72B
7B
8B
8�B
8�B
9	B
9>B
9XB
9rB
9�B
9�B
9�B
:B
:^B
:DB
:xB
:�B
:DB
:�B
;B
;B
:�B
:�B
;B
;dB
;dB
;�B
<B
;�B
;�B
<B
<�B
=B
=�B
=�B
=�B
=�B
=�B
>B
>B
=�B
>BB
>�B
>�B
>�B
>�B
?B
?B
?.B
?HB
?�B
@B
@ B
@ B
@4B
@ B
@OB
@iB
@�B
@�B
A;B
A B
A�B
A�B
AoB
A�B
A�B
A�B
B�B
CB
DB
D3B
DMB
D�B
D�B
D�B
EB
E9B
E�B
E�B
E�B
F?B
FtB
F?B
FYB
F�B
F�B
GEB
G_B
GzB
G�B
H�B
H�B
I7B
I7B
I�B
I�B
JrB
J�B
J�B
KxB
K^B
K�B
K�B
K�B
LB
LdB
LdB
LdB
L0B
LdB
MB
M6B
M6B
M�B
M�B
N"B
NpB
N�B
N�B
OB
O\B
O\B
O�B
O�B
PHB
PbB
P�B
Q�B
R:B
R�B
SB
S�B
T,B
TFB
T{B
T�B
T�B
UB
U�B
U�B
U�B
VB
VB
VB
VmB
V�B
V�B
V�B
V�B
W
B
W
B
W?B
W?B
W�B
XEB
XEB
XyB
XyB
X�B
X�B
Y1B
YeB
Y�B
YB
Z7B
ZkB
Z�B
Z�B
[=B
[�B
[�B
[�B
[�B
[�B
\)B
\�B
\�B
\�B
\�B
]B
]~B
]�B
]�B
]�B
^5B
^5B
^jB
^�B
^�B
^�B
_!B
_VB
_�B
`B
`B
`'B
`\B
`�B
a-B
a-B
abB
a�B
a|B
a�B
a�B
a�B
bhB
b�B
b�B
cTB
cnB
cnB
c�B
d&B
d@B
dZB
dZB
dZB
d�B
eB
d�B
eB
eB
e`B
e,B
e`B
e�B
e�B
f2B
ffB
f�B
f�B
gB
gRB
g�B
h
B
h
B
h$B
hXB
h�B
h�B
h�B
iDB
i�B
jB
jeB
j�B
j�B
j�B
kQB
kQB
k�B
k�B
k�B
k�B
lB
lWB
lWB
l�B
l�B
l�B
mB
m]B
mwB
mwB
m�B
m�B
m�B
m�B
n/B
nB
n}B
n�B
o B
o5B
o5B
o5B
o�B
o�B
o�B
o�B
pB
o�B
pB
p;B
poB
p�B
p�B
p�B
p�B
p�B
qB
qB
q'B
qAB
qAB
q'B
qAB
qvB
qvB
qvB
qvB
qvB
qvB
q�B
q�B
q�B
r-B
r|B
raB
r|B
r|B
r�B
r�B
r�B
r�B
r�B
r�B
s3B
s3B
sMB
s�B
s�B
s�B
s�B
s�B
s�B
s�B
tB
tnB
t9B
tnB
t�B
t�B
uB
u?B
uZB
utB
u�B
v+B
vzB
v�B
v�B
vzB
v�B
w2B
w�B
w�B
w�B
w�B
w�B
xRB
xlB
xlB
x�B
x�B
x�B
x�B
yrB
y�B
y�B
y�B
z*B
z*B
zxB
z�B
z�B
{B
{JB
{0B
{JB
{B
{�B
{�B
{�B
|B
|B
|PB
|6B
|jB
|�B
|�B
}"B
}�B
}�B
~B
~BB
~wB
~�B
~wB
~�B
~�B
~�B
B
B
B
cB
}B
�B
�B
� B
�B
�B
�OB
�iB
��B
��B
��B
�B
�;B
�;B
�oB
��B
��B
��B
�'B
��B
��B
��B
��B
�-B
�B
�GB
�GB
�aB
�{B
��B
��B
��B
��B
��B
�B
�MB
��B
��B
�B
�9B
�9B
�B
�9B
�9B
��B
��B
��B
�B
�%B
�%B
�YB
�?B
�t111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                            JA  ARFMdecpA30a                                                                20220604105240  IP                  G�O�G�O�G�O�                JA  ARFMfmtp2.8                                                                 20220604192143  IP                  G�O�G�O�G�O�                JA  ARCAsspa3.1b                                                                20220604192145  IP  PRES            G�O�G�O�G�O�                JA      jafc1.0                                                                 20220604192145                      G�O�G�O�G�O�                JA  ARGQrqcpc3.6                                                                20220605042153  QCP$                G�O�G�O�G�O�         20DF37EJA  ARGQrqcpc3.6                                                                20220605042153  QCF$                G�O�G�O�G�O�               0JA  ARUP                                                                        20220610151509                      G�O�G�O�G�O�                