CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       2019-12-22T11:00:28Z creation      
references        (http://www.argodatamgt.org/Documentation   comment       	free text      user_manual_version       3.2    Conventions       Argo-3.2 CF-1.6    featureType       trajectoryProfile         @   	DATA_TYPE                  	long_name         	Data type      conventions       Argo reference table 1     
_FillValue                    6�   FORMAT_VERSION                 	long_name         File format version    
_FillValue                    6�   HANDBOOK_VERSION               	long_name         Data handbook version      
_FillValue                    6�   REFERENCE_DATE_TIME                 	long_name         !Date of reference for Julian days      conventions       YYYYMMDDHHMISS     
_FillValue                    6�   DATE_CREATION                   	long_name         Date of file creation      conventions       YYYYMMDDHHMISS     
_FillValue                    6�   DATE_UPDATE                 	long_name         Date of update of this file    conventions       YYYYMMDDHHMISS     
_FillValue                    6�   PLATFORM_NUMBER                   	long_name         Float unique identifier    conventions       WMO float identifier : A9IIIII     
_FillValue                    6�   PROJECT_NAME                  	long_name         Name of the project    
_FillValue                  @  6�   PI_NAME                   	long_name         "Name of the principal investigator     
_FillValue                  @  7(   STATION_PARAMETERS           	            	long_name         ,List of available parameters for the station   conventions       Argo reference table 3     
_FillValue                  0  7h   CYCLE_NUMBER               	long_name         Float cycle number     conventions       =0...N, 0 : launch cycle (if exists), 1 : first complete cycle      
_FillValue         ��        7�   	DIRECTION                  	long_name         !Direction of the station profiles      conventions       -A: ascending profiles, D: descending profiles      
_FillValue                    7�   DATA_CENTRE                   	long_name         .Data centre in charge of float data processing     conventions       Argo reference table 4     
_FillValue                    7�   DC_REFERENCE                  	long_name         (Station unique identifier in data centre   conventions       Data centre convention     
_FillValue                     7�   DATA_STATE_INDICATOR                  	long_name         1Degree of processing the data have passed through      conventions       Argo reference table 6     
_FillValue                    7�   	DATA_MODE                  	long_name         Delayed mode or real time data     conventions       >R : real time; D : delayed mode; A : real time with adjustment     
_FillValue                    7�   PLATFORM_TYPE                     	long_name         Type of float      conventions       Argo reference table 23    
_FillValue                     7�   FLOAT_SERIAL_NO                   	long_name         Serial number of the float     
_FillValue                     7�   FIRMWARE_VERSION                  	long_name         Instrument firmware version    
_FillValue                     8   WMO_INST_TYPE                     	long_name         Coded instrument type      conventions       Argo reference table 8     
_FillValue                    8,   JULD               	long_name         ?Julian day (UTC) of the station relative to REFERENCE_DATE_TIME    standard_name         time   units         "days since 1950-01-01 00:00:00 UTC     conventions       8Relative julian days with decimal part (as parts of day)   
resolution        >�EȠ      
_FillValue        A.�~       axis      T           80   JULD_QC                	long_name         Quality on date and time   conventions       Argo reference table 2     
_FillValue                    88   JULD_LOCATION                  	long_name         @Julian day (UTC) of the location relative to REFERENCE_DATE_TIME   units         "days since 1950-01-01 00:00:00 UTC     conventions       8Relative julian days with decimal part (as parts of day)   
resolution        >�EȠ      
_FillValue        A.�~            8<   LATITUDE               	long_name         &Latitude of the station, best estimate     standard_name         latitude   units         degree_north   
_FillValue        @�i�       	valid_min         �V�        	valid_max         @V�        axis      Y           8D   	LONGITUDE                  	long_name         'Longitude of the station, best estimate    standard_name         	longitude      units         degree_east    
_FillValue        @�i�       	valid_min         �f�        	valid_max         @f�        axis      X           8L   POSITION_QC                	long_name         ,Quality on position (latitude and longitude)   conventions       Argo reference table 2     
_FillValue                    8T   POSITIONING_SYSTEM                    	long_name         Positioning system     
_FillValue                    8X   VERTICAL_SAMPLING_SCHEME                  	long_name         Vertical sampling scheme   conventions       Argo reference table 16    
_FillValue                    8`   CONFIG_MISSION_NUMBER                  	long_name         :Unique number denoting the missions performed by the float     conventions       !1...N, 1 : first complete mission      
_FillValue         ��        9`   PROFILE_PRES_QC                	long_name         #Global quality flag of PRES profile    conventions       Argo reference table 2a    
_FillValue                    9d   PROFILE_TEMP_QC                	long_name         #Global quality flag of TEMP profile    conventions       Argo reference table 2a    
_FillValue                    9h   PROFILE_PSAL_QC                	long_name         #Global quality flag of PSAL profile    conventions       Argo reference table 2a    
_FillValue                    9l   PRES         
      
   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���   axis      Z        D  9p   PRES_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  H�   PRES_ADJUSTED            
      	   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     D  L�   PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  [�   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         decibar    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     D  _�   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     D  n�   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ~(   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     D  ��   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �@   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         degree_Celsius     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     D  �   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     D  �X   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ��   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     D  �p   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ƴ   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     D  ʈ   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  0  ��   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                    ��   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                    ��   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                    ��   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  ,  ��   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                    �(   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                    �,   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                    �0   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                    �4   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  @  �8   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                    �x   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                    �   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    �   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar         �   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar         �   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�        �   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    �Argo profile    3.1 1.2 19500101000000  20191222110028  20191222110028  4902078 US ARGO PROJECT                                                 GREGORY C. JOHNSON                                              PRES            TEMP            PSAL               �A   AO  5439                            2B  A   NAVIS_A                         0460                            011514                          863 @��Y4��:1   @��[��ɀ@,��
=p��d����S�1   GPS     Primary sampling: averaged []                                                                                                                                                                                                                                      �A   A   A   @���@���A   A   A@  A`  A�  A�  A�  A�33A�  A�  A�  A�  B   B  B  B  B   B(  B0  B8ffB@  BHffBP  BX  B`  Bh  Bp  Bx  B��B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�33B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  C   C  C  C  C  C
  C  C  C  C  C  C  C  C  C  C  C   C"  C#�fC&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do�fDp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dt� Du  Du� Dv  Dv� Dw  Dw� Dx  Dx� Dy  Dy� Dz  Dz� D{  D{� D|  D|� D}  D}� D~  D~� D  D� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�<�D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D D�� D�  D�@ DÀ D�� D�  D�@ DĀ D�� D�  D�@ Dŀ D�� D�  D�@ Dƀ D�� D�  D�@ Dǀ D�� D�  D�@ DȀ D�� D�  D�@ Dɀ D�� D�  D�@ Dʀ D�� D�  D�@ Dˀ D�� D�  D�@ D̀ D�� D�  D�@ D̀ D�� D�  D�@ D΀ D�� D�  D�<�Dπ D�� D�  D�@ DЀ D�� D�  D�@ Dр D�� D�  D�@ DҀ D�� D�  D�@ DӀ D�� D�  D�@ DԀ D�� D�  D�@ DՀ D�� D�  D�@ Dր D�� D�  D�@ D׀ D�� D�  D�@ D؀ D�� D�  D�@ Dـ D�� D�  D�@ Dڀ D�� D�  D�@ Dۀ D�� D�  D�@ D܀ D�� D�  D�@ D݀ D�� D�  D�@ Dހ D�� D�  D�@ D߀ D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�<�D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�FfD�c311111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   @��@�
>A	�A)�AI�Ai�A��\A��\A��\A�Aď\Aԏ\A�\A�\BG�B
G�BG�BG�B"G�B*G�B2G�B:�BBG�BJ�BRG�BZG�BbG�BjG�BrG�BzG�B��B�#�B�#�B�#�B�#�B�#�B�#�B�#�B�#�B�#�B�#�B�#�B�#�B�#�B�W
B�#�B�#�B�#�B�#�B�#�B�#�B�#�B�#�B�#�B�#�B�#�B�#�B�#�B�#�B�#�B�#�B�#�C ��C��C��C��C��C
��C��C��C��C��C��C��C��C��C��C��C ��C"��C$xRC&��C(��C*��C,��C.��C0��C2��C4��C6��C8��C:��C<��C>��C@��CB��CD��CF��CH��CJ��CL��CN��CP��CR��CT��CV��CX��CZ��C\��C^��C`��Cb��Cd��Cf��Ch��Cj��Cl��Cn��Cp��Cr��Ct��Cv��Cx��Cz��C|��C~��C�H�C�H�C�H�C�H�C�H�C�H�C�H�C�H�C�H�C�H�C�H�C�H�C�H�C�H�C�H�C�H�C�H�C�H�C�H�C�H�C�H�C�H�C�H�C�H�C�H�C�H�C�H�C�H�C�H�C�H�C�H�C�H�C�H�C�H�C�H�C�H�C�H�C�H�C�H�C�H�C�H�C�H�C�H�C�H�C�H�C�H�C�H�C�H�C�H�C�H�C�H�C�H�C�H�C�H�C�H�C�H�C�H�C�H�C�H�C�H�C�H�C�H�C�H�C�H�C�H�C�H�C�H�C�H�C�H�C�H�C�H�C�H�C�H�C�H�C�H�C�H�C�H�C�H�C�H�C�H�C�H�C�H�C�H�C�H�C�H�C�H�C�H�C�H�C�H�C�H�C�H�C�H�C�H�C�H�C�H�C�H�C�H�C�H�C�H�C�H�C�H�C�H�C�H�C�H�C�H�C�H�C�H�C�H�C�H�C�H�C�H�C�H�C�H�C�H�C�H�C�H�C�H�C�H�C�H�C�H�C�H�C�H�C�H�C�H�C�H�C�H�C�H�C�H�D ${D �{D${D�{D${D�{D${D�{D${D�{D${D�{D${D�{D${D�{D${D�{D	${D	�{D
${D
�{D${D�{D${D�{D${D�{D${D�{D${D�{D${D�{D${D�{D${D�{D${D�{D${D�{D${D�{D${D�{D${D�{D${D�{D${D�{D${D�{D${D�{D${D�{D${D�{D${D�{D${D�{D ${D �{D!${D!�{D"${D"�{D#${D#�{D$${D$�{D%${D%�{D&${D&�{D'${D'�{D(${D(�{D)${D)�{D*${D*�{D+${D+�{D,${D,�{D-${D-�{D.${D.�{D/${D/�{D0${D0�{D1${D1�{D2${D2�{D3${D3�{D4${D4�{D5${D5�{D6${D6�{D7${D7�{D8${D8�{D9${D9�{D:${D:�{D;${D;�{D<${D<�{D=${D=�{D>${D>�{D?${D?�{D@${D@�{DA${DA�{DB${DB�{DC${DC�{DD${DD�{DE${DE�{DF${DF�{DG${DG�{DH${DH�{DI${DI�{DJ${DJ�{DK${DK�{DL${DL�{DM${DM�{DN${DN�{DO${DO�{DP${DP�{DQ${DQ�{DR${DR�{DS${DS�{DT${DT�{DU${DU�{DV${DV�{DW${DW�{DX${DX�{DY${DY�{DZ${DZ�{D[${D[�{D\${D\�{D]${D]�{D^${D^�{D_${D_�{D`${D`�{Da${Da�{Db${Db�{Dc${Dc�{Dd${Dd�{De${De�{Df${Df�{Dg${Dg�{Dh${Dh�{Di${Di�{Dj${Dj�{Dk${Dk�{Dl${Dl�{Dm${Dm�{Dn${Dn�{Do${Do��Dp${Dp�{Dq${Dq�{Dr${Dr�{Ds${Ds�{Dt${Dt�{Du${Du�{Dv${Dv�{Dw${Dw�{Dx${Dx�{Dy${Dy�{Dz${Dz�{D{${D{�{D|${D|�{D}${D}�{D~${D~�{D${D�{D�=D�R=D��=D��=D�=D�R=D��=D��=D�=D�R=D��=D��=D�=D�R=D��=D��=D�=D�R=D��=D��=D�=D�O
D��=D��=D�=D�R=D��=D��=D�=D�R=D��=D��=D�=D�R=D��=D��=D�=D�R=D��=D��=D�=D�R=D��=D��=D�=D�R=D��=D��=D�=D�R=D��=D��=D�=D�R=D��=D��=D�=D�R=D��=D��=D�=D�R=D��=D��=D�=D�R=D��=D��=D�=D�R=D��=D��=D�=D�R=D��=D��=D�=D�R=D��=D��=D�=D�R=D��=D��=D�=D�R=D��=D��=D�=D�R=D��=D��=D�=D�R=D��=D��=D�=D�R=D��=D��=D�=D�R=D��=D��=D�=D�R=D��=D��=D�=D�R=D��=D��=D�=D�R=D��=D��=D�=D�R=D��=D��=D�=D�R=D��=D��=D�=D�R=D��=D��=D�=D�R=D��=D��=D�=D�R=D��=D��=D�=D�R=D��=D��=D�=D�R=D��=D��=D�=D�R=D��=D��=D�=D�R=D��=D��=D�=D�R=D��=D��=D�=D�R=D��=D��=D�=D�R=D��=D��=D�=D�R=D��=D��=D�=D�R=D��=D��=D�=D�R=D��=D��=D�=D�R=D��=D��=D�=D�R=D��=D��=D�=D�R=D��=D��=D�=D�R=D��=D��=D�=D�R=D��=D��=D�=D�R=D��=D��=D�=D�R=D��=D��=D�=D�R=D��=D��=D�=D�R=D��=D��=D�=D�R=D��=D��=D�=D�R=D��=D��=D�=D�R=D��=D��=D�=D�R=D��=D��=D�=D�R=D��=D��=D�=D�R=D��=D��=D�=D�R=D��=D��=D�=D�R=D��=D��=D�=D�R=D��=D��=D�=D�R=D��=D��=D�=D�R=D��=D��=D�=D�R=D��=D��=D�=D�R=D��=D��=D�=D�R=D=D��=D�=D�R=DÒ=D��=D�=D�R=DĒ=D��=D�=D�R=DŒ=D��=D�=D�R=Dƒ=D��=D�=D�R=Dǒ=D��=D�=D�R=DȒ=D��=D�=D�R=Dɒ=D��=D�=D�R=Dʒ=D��=D�=D�R=D˒=D��=D�=D�R=D̒=D��=D�=D�R=D͒=D��=D�=D�R=DΒ=D��=D�=D�O
Dϒ=D��=D�=D�R=DВ=D��=D�=D�R=Dђ=D��=D�=D�R=DҒ=D��=D�=D�R=DӒ=D��=D�=D�R=DԒ=D��=D�=D�R=DՒ=D��=D�=D�R=D֒=D��=D�=D�R=Dג=D��=D�=D�R=Dؒ=D��=D�=D�R=Dْ=D��=D�=D�R=Dڒ=D��=D�=D�R=Dے=D��=D�=D�R=Dܒ=D��=D�=D�R=Dݒ=D��=D�=D�R=Dޒ=D��=D�=D�R=Dߒ=D��=D�=D�R=D��=D��=D�=D�R=D�=D��=D�=D�R=D�=D��=D�=D�R=D�=D��=D�=D�R=D�=D��=D�=D�R=D�=D��=D�=D�R=D�=D��=D�=D�O
D�=D��=D�=D�R=D�=D��=D�=D�R=D�=D��=D�=D�R=D�=D��=D�=D�R=D�=D��=D�=D�R=D�=D��=D�=D�R=D�=D��=D�=D�R=D�=D��=D�=D�R=D�=D��=D�=D�R=D�=D��=D�=D�R=D�=D��=D�=D�R=D�=D��=D�=D�R=D�=D��=D�=D�X�D�up11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�A�^5A�`BA�`BA�^5A�`BA�`BA�^5A�`BA�`BA�bNA�jA�jA�l�A�p�A�n�A�n�A�n�A�r�A�x�A�z�A�|�A�z�A�z�A�|�AہAہAہA�|�A�p�A�Q�A���Aײ-AӺ^A�;dA�-A�A�A��A�`BA�VA���AöFA��A���A��!A�7LA�ffA�hsA��A�\)A��A�7LA��A�dZA��mA���A��
A��A�ĜA�G�A���A���A��A�l�A�?}A���A�O�A���A��A�{A��uA��A���A��A}�#Av�!Ap��Am;dAgx�A`�`A]l�AW�hAQ;dAPz�AP$�AO�^AN��AM�;AM&�AL�ALM�AK��AK;dAJ�jAGƨAE�AC�A@�RA?S�A>(�A;��A6�A6�\A6M�A57LA3�A3O�A2�A2n�A2�A1��A0�HA0E�A0M�A0(�A/��A0  A/C�A.n�A.^5A.VA/&�A1VA1|�A1�A1+A0��A/�TA.�\A.JA-�A+&�A)��A(�+A&�A%?}A$Q�A#%A�wA
=AJAXAI�A��A�-AS�A�/A�DAhsA�A �A��A|�A+A�uAƨA+A�;A�PA�wA��AA
�/A
�A
�`A
�HA
��A	�A��A�mA�wA+A9XA�A��A+A�yA"�A~�A�FA?}A�HA�9A�AȴA�A��A�#AG�A�hAx�A ��A jA  �@�ff@�&�@���@�J@���@��\@�E�@��T@�-@��7@�\)@��@���@�7L@�D@�@�\)@��@�@�+@�~�@�5?@�{@�J@�^@���@�@�1'@�@�o@��@�b@��m@�;d@��@�
=@���@�ȴ@�+@��#@�O�@�9@� �@�1@�  @�@�-@�X@蛦@�bN@�%@���@�bN@�  @�F@�\)@�^5@噚@�(�@�\)@��@�@��m@�\)@�@�;d@�l�@�l�@�S�@�K�@�dZ@�dZ@ޗ�@�$�@ݩ�@��@��@ڇ+@�$�@ٺ^@�x�@�/@��/@�r�@���@ו�@�+@�
=@֟�@��@ղ-@�p�@�?}@��@��`@���@ӶF@�;d@�ff@���@�`B@���@У�@�z�@�(�@θR@�V@���@��@��@�@��@͙�@�x�@�X@��@��`@̛�@�I�@��@��;@˾w@ˍP@�l�@�;d@ʧ�@�?}@�1@�;d@�
=@Ə\@��#@�p�@��/@�9X@��m@î@�S�@�"�@���@�n�@�E�@�{@��@���@��h@�p�@��@���@�z�@��@�33@�=q@�p�@���@���@���@�r�@�1'@��;@���@�t�@��H@�V@��#@��7@��@���@��j@���@�Q�@�1@�|�@���@��^@���@�bN@���@��@�;d@�"�@�@��@���@���@��@��^@��-@��7@��@��@��`@��`@��`@��/@��9@���@��D@�bN@�  @�;d@�-@���@���@���@�@�x�@��@�%@���@�z�@��@�dZ@�S�@�"�@��!@�n�@�$�@��@���@�p�@��@��9@�z�@�b@��P@�K�@�
=@��@��!@�ff@��#@�@���@�X@���@�j@�9X@� �@��;@�l�@�+@���@�ff@��7@�/@�V@��`@��/@��/@��j@��u@��@�j@�Q�@�1'@��@�  @�ƨ@��@��@��@��R@�~�@�{@���@��@�p�@�hs@�X@�G�@�G�@�&�@���@��u@�Q�@���@�ƨ@�S�@�;d@�;d@�;d@�33@�
=@���@���@���@��\@�ff@���@�hs@��@��@�b@��@�l�@�K�@�;d@��@��H@�V@��@�G�@�7L@�7L@�7L@��@��j@�r�@� �@��m@��m@��
@���@��w@�|�@�S�@�@��y@���@���@�ff@�J@���@��@��@���@��9@��@�z�@�I�@��
@�K�@��@��@�o@��@�^5@�@��@�Ĝ@��@�I�@�(�@��@�|�@�o@��@��y@��H@�ȴ@��+@�^5@�=q@�-@��@�@�@��@���@�?}@��@���@��@��@�S�@�"�@��H@��!@�n�@�=q@�J@��@���@��^@���@�x�@�G�@��@���@��j@� �@�t�@�"�@���@�v�@�V@�E�@�5?@�{@��T@��h@���@��@�@|�@+@~��@}��@}?}@|��@{33@z�H@z��@z��@z��@z�\@z-@y�^@y&�@x��@xr�@xA�@w��@w|�@wl�@w\)@wK�@w�@v�y@vȴ@v5?@up�@t�@t�@s�F@st�@sS�@so@r��@r�\@r~�@r~�@rn�@r^5@rM�@q�#@q��@qhs@p�u@p �@o\)@n��@n�@n�R@n��@nv�@nV@m��@l��@lZ@k�
@kS�@j�@j�!@j~�@i��@ihs@i&�@h��@h��@hQ�@g�;@g�w@g�P@g|�@gl�@g
=@fȴ@fv�@f{@e@e@e@e�h@eV@d(�@c��@c33@b�\@bJ@a�^@a��@aX@`��@`�u@`b@_��@_��@_|�@_\)@_K�@_;d@_�@^ȴ@^ff@^{@]��@\�@\�D@\j@\(�@[�m@[�F@[t�@["�@Z�@Z�\@ZJ@Y��@Y��@YX@Y�@X�9@X �@X  @W�;@W�P@W+@Vȴ@V��@V��@Vv�@U@UO�@UV@T�/@T��@Tj@TI�@S�F@S�@S"�@R^5@QX@Q%@P��@PQ�@O�@N��@N{@M@M�@MV@L�@L�/@L��@L�D@L9X@KC�@J��@I��@I%@H�9@HbN@G�;@G\)@G+@G
=@F��@F�y@Fȴ@Fv�@Fff@F$�@E�T@E��@E�h@E�@Ep�@D�/@Ct�@C@B�H@B�!@Bn�@B=q@B-@A��@A�7@Ahs@Ahs@A�@@�@@1'@@ �@@  @?��@?l�@>�R@>5?@>@=�T@=�T@=�T@=�h@<�@;�
@;ƨ@;�F@;��@;dZ@;C�@;o@:�H@:=q@9��@9��@9hs@9�@8�9@8A�@7�;@7��@7�@7\)@6V@5�-@5p�@5O�@5/@4�@4�@4�j@4z�@4(�@3�F@3��@3"�@3@2�@2��@2�!@2��@2n�@1�#@1�7@1G�@1�@0��@0�`@0Ĝ@0�9@0��@0�@0r�@0 �@/|�@.�@.��@.V@-�@-�-@-O�@-O�@-�@,�j@,Z@+��@+dZ@*�!@*M�@)��@)��@)�^@)��@)��@)G�@)%@(�9@(bN@'�@';d@&��@&�@&ȴ@&��@&��@&�+@&E�@&@%�h@%V@$��@$�j@$(�@#��@#�F@#dZ@#33@"�@"��@"=q@!�#@!x�@ �9@ bN@ A�@ b@   @�w@�P@K�@�@��@��@�y@ȴ@��@�+@�+@v�@5?@�@@��@`B@V@V@�@�j@�@�D@Z@�
@S�@��@��@^5@-@�@�7@7L@�@%@��@��@Q�@1'@��@K�@�@ȴ@�R@E�@p�@?}@V@��@I�@(�@(�@�@1@1@1@�m@�F@�@dZ@C�@33@"�@o@�@��@�\@~�@n�@=q@�#@�^@�7@hs@G�@�@�`@��@��@Ĝ@�9@�@1'@  11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   A�^5A�`BA�`BA�^5A�`BA�`BA�^5A�`BA�`BA�bNA�jA�jA�l�A�p�A�n�A�n�A�n�A�r�A�x�A�z�A�|�A�z�A�z�A�|�AہAہAہA�|�A�p�A�Q�A���Aײ-AӺ^A�;dA�-A�A�A��A�`BA�VA���AöFA��A���A��!A�7LA�ffA�hsA��A�\)A��A�7LA��A�dZA��mA���A��
A��A�ĜA�G�A���A���A��A�l�A�?}A���A�O�A���A��A�{A��uA��A���A��A}�#Av�!Ap��Am;dAgx�A`�`A]l�AW�hAQ;dAPz�AP$�AO�^AN��AM�;AM&�AL�ALM�AK��AK;dAJ�jAGƨAE�AC�A@�RA?S�A>(�A;��A6�A6�\A6M�A57LA3�A3O�A2�A2n�A2�A1��A0�HA0E�A0M�A0(�A/��A0  A/C�A.n�A.^5A.VA/&�A1VA1|�A1�A1+A0��A/�TA.�\A.JA-�A+&�A)��A(�+A&�A%?}A$Q�A#%A�wA
=AJAXAI�A��A�-AS�A�/A�DAhsA�A �A��A|�A+A�uAƨA+A�;A�PA�wA��AA
�/A
�A
�`A
�HA
��A	�A��A�mA�wA+A9XA�A��A+A�yA"�A~�A�FA?}A�HA�9A�AȴA�A��A�#AG�A�hAx�A ��A jA  �@�ff@�&�@���@�J@���@��\@�E�@��T@�-@��7@�\)@��@���@�7L@�D@�@�\)@��@�@�+@�~�@�5?@�{@�J@�^@���@�@�1'@�@�o@��@�b@��m@�;d@��@�
=@���@�ȴ@�+@��#@�O�@�9@� �@�1@�  @�@�-@�X@蛦@�bN@�%@���@�bN@�  @�F@�\)@�^5@噚@�(�@�\)@��@�@��m@�\)@�@�;d@�l�@�l�@�S�@�K�@�dZ@�dZ@ޗ�@�$�@ݩ�@��@��@ڇ+@�$�@ٺ^@�x�@�/@��/@�r�@���@ו�@�+@�
=@֟�@��@ղ-@�p�@�?}@��@��`@���@ӶF@�;d@�ff@���@�`B@���@У�@�z�@�(�@θR@�V@���@��@��@�@��@͙�@�x�@�X@��@��`@̛�@�I�@��@��;@˾w@ˍP@�l�@�;d@ʧ�@�?}@�1@�;d@�
=@Ə\@��#@�p�@��/@�9X@��m@î@�S�@�"�@���@�n�@�E�@�{@��@���@��h@�p�@��@���@�z�@��@�33@�=q@�p�@���@���@���@�r�@�1'@��;@���@�t�@��H@�V@��#@��7@��@���@��j@���@�Q�@�1@�|�@���@��^@���@�bN@���@��@�;d@�"�@�@��@���@���@��@��^@��-@��7@��@��@��`@��`@��`@��/@��9@���@��D@�bN@�  @�;d@�-@���@���@���@�@�x�@��@�%@���@�z�@��@�dZ@�S�@�"�@��!@�n�@�$�@��@���@�p�@��@��9@�z�@�b@��P@�K�@�
=@��@��!@�ff@��#@�@���@�X@���@�j@�9X@� �@��;@�l�@�+@���@�ff@��7@�/@�V@��`@��/@��/@��j@��u@��@�j@�Q�@�1'@��@�  @�ƨ@��@��@��@��R@�~�@�{@���@��@�p�@�hs@�X@�G�@�G�@�&�@���@��u@�Q�@���@�ƨ@�S�@�;d@�;d@�;d@�33@�
=@���@���@���@��\@�ff@���@�hs@��@��@�b@��@�l�@�K�@�;d@��@��H@�V@��@�G�@�7L@�7L@�7L@��@��j@�r�@� �@��m@��m@��
@���@��w@�|�@�S�@�@��y@���@���@�ff@�J@���@��@��@���@��9@��@�z�@�I�@��
@�K�@��@��@�o@��@�^5@�@��@�Ĝ@��@�I�@�(�@��@�|�@�o@��@��y@��H@�ȴ@��+@�^5@�=q@�-@��@�@�@��@���@�?}@��@���@��@��@�S�@�"�@��H@��!@�n�@�=q@�J@��@���@��^@���@�x�@�G�@��@���@��j@� �@�t�@�"�@���@�v�@�V@�E�@�5?@�{@��T@��h@���@��@�@|�@+@~��@}��@}?}@|��@{33@z�H@z��@z��@z��@z�\@z-@y�^@y&�@x��@xr�@xA�@w��@w|�@wl�@w\)@wK�@w�@v�y@vȴ@v5?@up�@t�@t�@s�F@st�@sS�@so@r��@r�\@r~�@r~�@rn�@r^5@rM�@q�#@q��@qhs@p�u@p �@o\)@n��@n�@n�R@n��@nv�@nV@m��@l��@lZ@k�
@kS�@j�@j�!@j~�@i��@ihs@i&�@h��@h��@hQ�@g�;@g�w@g�P@g|�@gl�@g
=@fȴ@fv�@f{@e@e@e@e�h@eV@d(�@c��@c33@b�\@bJ@a�^@a��@aX@`��@`�u@`b@_��@_��@_|�@_\)@_K�@_;d@_�@^ȴ@^ff@^{@]��@\�@\�D@\j@\(�@[�m@[�F@[t�@["�@Z�@Z�\@ZJ@Y��@Y��@YX@Y�@X�9@X �@X  @W�;@W�P@W+@Vȴ@V��@V��@Vv�@U@UO�@UV@T�/@T��@Tj@TI�@S�F@S�@S"�@R^5@QX@Q%@P��@PQ�@O�@N��@N{@M@M�@MV@L�@L�/@L��@L�D@L9X@KC�@J��@I��@I%@H�9@HbN@G�;@G\)@G+@G
=@F��@F�y@Fȴ@Fv�@Fff@F$�@E�T@E��@E�h@E�@Ep�@D�/@Ct�@C@B�H@B�!@Bn�@B=q@B-@A��@A�7@Ahs@Ahs@A�@@�@@1'@@ �@@  @?��@?l�@>�R@>5?@>@=�T@=�T@=�T@=�h@<�@;�
@;ƨ@;�F@;��@;dZ@;C�@;o@:�H@:=q@9��@9��@9hs@9�@8�9@8A�@7�;@7��@7�@7\)@6V@5�-@5p�@5O�@5/@4�@4�@4�j@4z�@4(�@3�F@3��@3"�@3@2�@2��@2�!@2��@2n�@1�#@1�7@1G�@1�@0��@0�`@0Ĝ@0�9@0��@0�@0r�@0 �@/|�@.�@.��@.V@-�@-�-@-O�@-O�@-�@,�j@,Z@+��@+dZ@*�!@*M�@)��@)��@)�^@)��@)��@)G�@)%@(�9@(bN@'�@';d@&��@&�@&ȴ@&��@&��@&�+@&E�@&@%�h@%V@$��@$�j@$(�@#��@#�F@#dZ@#33@"�@"��@"=q@!�#@!x�@ �9@ bN@ A�@ b@   @�w@�P@K�@�@��@��@�y@ȴ@��@�+@�+@v�@5?@�@@��@`B@V@V@�@�j@�@�D@Z@�
@S�@��@��@^5@-@�@�7@7L@�@%@��@��@Q�@1'@��@K�@�@ȴ@�R@E�@p�@?}@V@��@I�@(�@(�@�@1@1@1@�m@�F@�@dZ@C�@33@"�@o@�@��@�\@~�@n�@=q@�#@�^@�7@hs@G�@�@�`@��@��@Ĝ@�9@�@1'@  11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	 �B	 �B	 �B	!�B	#�B	(�B	1'B	bNB	�B
)�B
�B
�B
5?B
H�B
Q�B
p�B
�B
��B
�!B
ǮB
��B
��B
ǮB
ǮB
��B
��B
�
B
�
B
�B
�B
��B
�BB
��BB1B%B!�BB
�LB
��B
�B
u�B
_;B
O�B
>wB
0!B
�B	��B	��B	�'B	�oB	{�B	dZB	M�B	@�B	49B	-B	-B	1'B	7LB	8RB	7LB	5?B	2-B	0!B	-B	)�B	$�B	{B	1B��B�B�B�yB�NB�B�B�B�#B�ZB�fB�B�B�B��B��B	B	JB	hB	oB	%�B	,B	2-B	@�B	Q�B	q�B	�B	�jB	ƨB	ǮB	ǮB	ȴB	��B	��B	��B	B	�RB	�!B	��B	��B	��B	�bB	|�B	p�B	o�B	q�B	{�B	�B	�{B	��B	��B	��B	��B	�dB	ɺB	�B	�#B	�)B	�TB	�HB	�5B	�B	��B	��B	��B	B	B	ÖB	ĜB	ĜB	B	�XB	�?B	�FB	�XB	�wB	�}B	ȴB	ÖB	��B	ÖB	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�
B	�/B	�sB	�B	�B	�yB	�mB	�BB	�#B	�`B	�B	�TB	�HB	�HB	�HB	�ZB	�`B	�NB	�HB	�BB	�NB	�TB	�ZB	�`B	�`B	�`B	�`B	�`B	�`B	�`B	�fB	�fB	�B	�B	�B	�B	�B	��B	��B
  B
  B
  B
B
B
B
B
B
B
B
B
1B
	7B

=B
+B
%B
B
1B
\B
{B
�B
�B
�B
�B
�B
uB
bB
\B
PB
1B
%B
%B
+B
JB
bB
uB
uB
uB
�B
�B
�B
�B
�B
�B
�B
�B
�B
{B
{B
{B
{B
uB
oB
oB
uB
{B
�B
�B
{B
{B
{B
{B
{B
{B
{B
{B
uB
{B
{B
{B
{B
�B
�B
uB
oB
hB
oB
{B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
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
#�B
$�B
$�B
$�B
$�B
$�B
%�B
%�B
%�B
%�B
%�B
%�B
%�B
%�B
%�B
%�B
&�B
&�B
&�B
&�B
&�B
'�B
'�B
'�B
'�B
'�B
'�B
(�B
(�B
'�B
(�B
(�B
(�B
(�B
(�B
(�B
)�B
)�B
)�B
)�B
)�B
+B
+B
+B
,B
,B
,B
,B
,B
+B
+B
+B
+B
+B
,B
-B
.B
.B
/B
/B
1'B
2-B
2-B
2-B
2-B
2-B
2-B
2-B
2-B
2-B
33B
49B
33B
49B
49B
49B
33B
49B
5?B
5?B
5?B
5?B
5?B
6FB
6FB
5?B
6FB
6FB
6FB
6FB
7LB
7LB
8RB
8RB
8RB
8RB
8RB
8RB
8RB
8RB
8RB
8RB
9XB
9XB
9XB
9XB
9XB
9XB
9XB
:^B
:^B
9XB
9XB
9XB
8RB
8RB
7LB
7LB
7LB
8RB
8RB
8RB
8RB
8RB
9XB
9XB
8RB
8RB
9XB
9XB
9XB
:^B
:^B
;dB
;dB
;dB
<jB
=qB
>wB
>wB
?}B
?}B
@�B
@�B
@�B
@�B
@�B
@�B
@�B
@�B
@�B
@�B
@�B
@�B
A�B
A�B
A�B
A�B
C�B
D�B
D�B
D�B
D�B
D�B
D�B
D�B
D�B
D�B
D�B
D�B
F�B
F�B
F�B
F�B
G�B
H�B
H�B
H�B
J�B
J�B
J�B
J�B
J�B
J�B
J�B
J�B
K�B
K�B
K�B
K�B
K�B
K�B
L�B
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
N�B
N�B
N�B
N�B
N�B
N�B
N�B
N�B
N�B
N�B
N�B
N�B
O�B
O�B
O�B
O�B
O�B
O�B
O�B
N�B
N�B
N�B
N�B
N�B
N�B
O�B
P�B
P�B
O�B
P�B
P�B
P�B
P�B
P�B
Q�B
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
R�B
R�B
S�B
S�B
T�B
T�B
T�B
VB
VB
VB
VB
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
W
B
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
YB
YB
YB
YB
YB
YB
ZB
ZB
ZB
ZB
ZB
ZB
ZB
ZB
[#B
[#B
[#B
[#B
[#B
[#B
[#B
[#B
[#B
[#B
\)B
\)B
\)B
\)B
\)B
\)B
\)B
\)B
]/B
]/B
^5B
^5B
_;B
_;B
_;B
_;B
`BB
`BB
`BB
aHB
aHB
aHB
aHB
aHB
aHB
bNB
bNB
bNB
cTB
cTB
cTB
dZB
dZB
dZB
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
gmB
gmB
gmB
gmB
gmB
gmB
hsB
hsB
hsB
hsB
hsB
hsB
iyB
iyB
iyB
iyB
iyB
iyB
jB
k�B
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
m�B
m�B
m�B
m�B
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
p�B
p�B
q�B
q�B
q�B
q�B
q�B
q�B
q�B
q�B
r�B
r�B
r�B
r�B
r�B
r�B
s�B
s�B
s�B
s�B
t�B
t�B
t�B
u�B
u�B
u�B
u�B
u�B
u�B
u�B
u�B
u�B
u�B
v�B
v�B
v�B
v�B
v�B
v�B
v�B
v�B
w�B
w�B
w�B
x�B
x�B
x�B
x�B
y�B
x�B
x�B
y�B
y�B
y�B
y�B
z�B
{�B
{�B
{�B
{�B
{�B
{�B
{�B
{�B
|�B
|�B
|�B
|�B
|�B
}�B
}�B
}�B
~�B
~�B
~�B
~�B
� B
� B
� B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�%B
�%B
�%B
�%B
�+B
�+B
�+B
�+B
�+B
�+B
�+B
�1B
�1B
�1B
�1B
�1B
�1B
�7B
�7B
�7B
�7B
�7B
�7B
�7B
�7B
�=B
�=B
�=B
�DB
�DB
�DB
�DB
�DB
�JB
�JB
�JB
�JB
�JB
�JB
�JB
�JB
�PB
�PB
�PB
�PB
�PB
�PB
�PB
�VB
�VB
�VB
�VB
�VB
�VB
�\B
�b11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   B	
UB	[B	
UB	
UB	
UB	
UB	HB		OB	
UB	
UB	aB	[B	aB	gB	aB	[B	[B	aB	sB	sB	sB	sB	sB	sB	zB	zB	zB	�B	�B	�B	 �B	RB	��B
�B
[B
HB
$�B
8iB
A�B
`YB
s�B
�sB
��B
�cB
�vB
��B
�cB
�cB
��B
íB
ƿB
ƿB
��B
��B
��B
�@B
��B
�qB
�B
��B
��B�B
�B
�B
�<B
q�B
exB
N�B
?�B
.,B
�B
6B	�B	ĳB	��B	�$B	k�B	TB	=�B	08B	#�B	�B	�B	 �B	'B	(B	'B	$�B	!�B	�B	�B	�B	�B	0B��B��B�kB�MB�.B�B��B��B��B��B�B�B�4B�FB�YB�qB�B�B��B	B	$B	�B	�B	!�B	08B	A�B	a_B	��B	�B	�]B	�cB	�cB	�iB	��B	��B	�vB	�DB	�B	��B	��B	�mB	�[B	�B	l�B	`YB	_SB	a_B	k�B	q�B	�0B	��B	��B	��B	��B	�B	�oB	��B	��B	��B	�	B	��B	��B	��B	��B	��B	��B	�DB	�DB	�KB	�QB	�QB	�DB	�B	��B	��B	�B	�,B	�2B	�iB	�KB	�>B	�KB	�|B	§B	íB	§B	��B	§B	ŹB	��B	��B	��B	ƿB	��B	�(B	�:B	�4B	�.B	�"B	��B	��B	�B	�4B	�	B	��B	��B	��B	�B	�B	�B	��B	��B	�B	�	B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�4B	�FB	�FB	�FB	�_B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B
0B
BB
<B
6B
<B
6B
*B
 B	�B	�B	��B	��B	��B	��B	��B
 B
*B
*B
*B
6B
BB
HB
HB
BB
<B
<B
6B
6B
0B
0B
0B
0B
*B
$B
$B
*B
0B
6B
6B
0B
0B
0B
0B
0B
0B
0B
0B
*B
0B
0B
0B
0B
6B
6B
*B
$B
B
$B
0B
<B

UB
[B
[B

UB

UB

UB

UB

UB

UB

UB

UB

UB

UB
	OB
	OB

UB

UB

UB

UB
	OB

UB
	OB
	OB
	OB
	OB
	OB
	OB
	OB
	OB
	OB
	OB
	OB
	OB
	OB
	OB
	OB
	OB
	OB
	OB
	OB
	OB
	OB

UB

UB

UB

UB

UB
	OB
	OB
	OB
	OB
HB
HB
HB
HB
HB
	OB
	OB
	OB

UB

UB
	OB
HB
HB
	OB

UB
	OB
	OB
	OB
	OB
	OB
	OB

UB
	OB

UB

UB

UB

UB

UB

UB

UB
[B
[B
[B
[B
[B
[B
[B
[B
[B
gB
gB
gB
gB
gB
gB
mB
mB
mB
mB
sB
sB
sB
sB
sB
sB
zB
zB
zB
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
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
#�B
"�B
#�B
#�B
#�B
"�B
#�B
$�B
$�B
$�B
$�B
$�B
%�B
%�B
$�B
%�B
%�B
%�B
%�B
'B
'B
(B
(B
(B
(B
(B
(B
(B
(B
(B
(B
)B
)B
)B
)B
)B
)B
)B
*B
*B
)B
)B
)B
(B
(B
'B
'B
'B
(B
(B
(B
(B
(B
)B
)B
(B
(B
)B
)B
)B
*B
*B
+B
+B
+B
,B
-&B
.,B
.,B
/2B
/2B
08B
08B
08B
08B
08B
08B
08B
08B
08B
08B
08B
08B
1>B
1>B
1>B
1>B
3KB
4QB
4QB
4QB
4QB
4QB
4QB
4QB
4QB
4QB
4QB
4QB
6]B
6]B
6]B
6]B
7cB
8iB
8iB
8iB
:vB
:vB
:vB
:vB
:vB
:vB
:vB
:vB
;|B
;|B
;|B
;|B
;|B
;|B
<�B
<�B
<�B
<�B
<�B
<�B
<�B
=�B
=�B
>�B
>�B
>�B
>�B
>�B
>�B
>�B
>�B
>�B
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
?�B
?�B
?�B
>�B
>�B
>�B
>�B
>�B
>�B
?�B
@�B
@�B
?�B
@�B
@�B
@�B
@�B
@�B
A�B
A�B
A�B
A�B
A�B
A�B
B�B
B�B
B�B
B�B
C�B
C�B
B�B
B�B
C�B
C�B
D�B
D�B
D�B
E�B
E�B
E�B
E�B
E�B
E�B
F�B
F�B
F�B
F�B
F�B
F�B
F�B
F�B
F�B
G�B
G�B
G�B
H�B
H�B
H�B
H�B
H�B
H�B
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
J�B
J�B
J�B
J�B
J�B
J�B
J�B
J�B
J�B
J�B
K�B
K�B
K�B
K�B
K�B
K�B
K�B
K�B
L�B
L�B
M�B
M�B
N�B
N�B
N�B
N�B
O�B
O�B
O�B
P�B
P�B
P�B
P�B
P�B
P�B
RB
RB
RB
S	B
S	B
S	B
TB
TB
TB
TB
TB
TB
TB
UB
UB
UB
UB
UB
UB
UB
UB
UB
W"B
W"B
W"B
W"B
W"B
W"B
X(B
X(B
X(B
X(B
X(B
X(B
Y.B
Y.B
Y.B
Y.B
Y.B
Y.B
Z4B
[:B
[:B
[:B
[:B
[:B
[:B
[:B
\@B
\@B
\@B
\@B
\@B
]FB
]FB
]FB
]FB
^MB
^MB
^MB
^MB
_SB
_SB
_SB
_SB
_SB
_SB
`YB
`YB
a_B
a_B
a_B
a_B
a_B
a_B
a_B
a_B
beB
beB
beB
beB
beB
beB
ckB
ckB
ckB
ckB
dqB
dqB
dqB
exB
exB
exB
exB
exB
exB
exB
exB
exB
exB
f~B
f~B
f~B
f~B
f~B
f~B
f~B
f~B
g�B
g�B
g�B
h�B
h�B
h�B
h�B
i�B
h�B
h�B
i�B
i�B
i�B
i�B
j�B
k�B
k�B
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
m�B
m�B
m�B
n�B
n�B
n�B
n�B
o�B
o�B
o�B
p�B
p�B
p�B
p�B
p�B
q�B
q�B
q�B
q�B
q�B
q�B
q�B
q�B
r�B
r�B
r�B
q�B
r�B
r�B
r�B
r�B
r�B
r�B
r�B
r�B
s�B
s�B
s�B
s�B
s�B
t�B
t�B
t�B
u�B
u�B
u�B
u�B
v�B
v�B
v�B
v�B
v�B
v�B
v�B
w�B
w�B
w�B
w�B
w�B
w�B
x�B
x�B
x�B
x�B
x�B
x�B
x�B
x�B
y�B
y�B
y�B
z�B
z�B
z�B
z�B
z�B
{�B
{�B
{�B
{�B
{�B
{�B
{�B
{�B
}B
}B
}B
}B
}B
}B
}B
~B
~B
~B
~B
~B
~B
B
�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�PRES            TEMP            PSAL            PRES_ADJUSTED = PRES - surface_pressure                                                                                                                                                                                                                         none                                                                                                                                                                                                                                                            PSAL_ADJUSTED = salinity + salinity_offset                                                                                                                                                                                                                      surface_pressure=-0.57 dbar                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                     salinity_offset = -0.0159092                                                                                                                                                                                                                                    Pressure adjusted at real time based on most recent valid surface pressure                                                                                                                                                                                                                                                                                                                                                                                                                                                      PSAL ADJUST [dd mm yyyy N S_off stddev] 15 06 2019 151 -0.0159092 0.0003 where N is the number of the delayed-mode profile used to estimate S_off stddev                                                                                                        20191222110028              20191222110028  AO  ARCAADJP                                                                    20191222110028    IP                G�O�G�O�G�O�                AO  ARCAADJS                                                                    20191222110028    IP                G�O�G�O�G�O�                AO  ARGQQCPL                                                                    20191222110028  QCP$                G�O�G�O�G�O�5F03E           AO  ARGQQCPL                                                                    20191222110028  QCF$                G�O�G�O�G�O�0               