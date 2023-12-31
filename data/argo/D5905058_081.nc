CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       JMA    source        
Argo float     history       b2018-08-11T15:35:19Z creation;2018-08-11T15:35:22Z conversion to V3.1;2019-12-23T06:17:05Z update;     
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
_FillValue                 �  I@   PRES_ADJUSTED            
      	   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  M,   PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  \�   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     
_FillValue        G�O�   	valid_min         �      	valid_max         B      C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  `�   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  p`   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     
_FillValue        G�O�   	valid_min         �      	valid_max         B      C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  tL   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ��   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    
_FillValue        G�O�   	valid_min         @      	valid_max         B$     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  ��   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ��   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    
_FillValue        G�O�   	valid_min         @      	valid_max         B$     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  �l   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         decibar    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  ��   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         degree_Celsius     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  ��   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  �D   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  `  ��   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                    �H   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                    �H   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                    �H   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  T  �H   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                    �   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                    �   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                    �   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                    �   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  @  �   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                    ��   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                    ��   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    �    HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar         �   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar         �   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�        �   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    �Argo profile    3.1 1.2 19500101000000  20180811153519  20200120021521  5905058                                                                 JAMSTEC                                                         PRES            TEMP            PSAL               QA   JA  I2_0675_081                     2C  D   NAVIS_A                         0675                            ARGO 102115                     863 @�x۟� 1   @�x�Q�n @8f�}Vl��c-�s�h1   GPS     A   A   A   Primary sampling: averaged [bin average for 1 Hz CTD]                                                                                                                                                                                                              @���@���A   A   A@  A`  A�  A�  A�  A�  A�  A�  A�  A�  B   B  B  B  B   B(  B0ffB7��B?��BH  BP  BX  B`  Bh  Bp  Bx  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  C   C  C  C  C  C
  C  C  C  C  C  C  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Ce�fCh  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$y�D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� DifDi� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn�fDo  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dt� Du  Du� Dv  Dv� Dw  Dw� Dx  Dx� Dy  Dy� Dz  Dz� D{  D{� D|  D|� D}  D}� D~  D~� D  D� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D��3D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D D�� D�  D�@ DÀ D�� D�  D�@ DĀ D�� D�  D�@ Dŀ D�� D�  D�@ Dƀ D�� D�  D�@ Dǀ D�� D�  D�@ DȀ D�� D�  D�@ Dɀ D�� D�  D�@ Dʀ D�� D�  D�@ Dˀ D�� D�  D�@ D̀ D�� D�  D�@ D̀ D�� D�  D�@ D΀ D�� D�  D�@ Dπ D�� D�  D�@ DЀ D�� D�  D�@ Dр D�� D�  D�@ DҀ D�� D�  D�@ DӀ D�� D�  D�@ DԀ D�� D�  D�@ DՀ D�� D�  D�@ Dր D�� D�  D�@ D׀ D�� D�  D�@ D؀ D�� D�  D�@ Dـ D�� D�  D�@ Dڀ D�� D�  D�@ Dۀ D�� D�  D�@ D܀ D�� D�  D�@ D݀ D�� D�  D�@ Dހ D�� D�  D�@ D߀ D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�C3D�l�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   @��
@�
=A	�A)�AI�Ai�A��\A��\A��\A��\Aď\Aԏ\A�\A�\BG�B
G�BG�BG�B"G�B*G�B2�B9�HBA�HBJG�BRG�BZG�BbG�BjG�BrG�BzG�B�#�B�#�B�#�B�#�B�#�B�#�B�#�B�#�B�#�B�#�B�#�B�#�B�#�B�#�B�#�B�#�B�#�B�#�B�#�B�#�B�#�B�#�B�#�B�#�B�#�B�#�B�#�B�#�B�#�B�#�B�#�B�#�C ��C��C��C��C��C
��C��C��C��C��C��C��C��C��C��C��C ��C"��C$��C&��C(��C*��C,��C.��C0��C2��C4��C6��C8��C:��C<��C>��C@��CB��CD��CF��CH��CJ��CL��CN��CP��CR��CT��CV��CX��CZ��C\��C^��C`��Cb��Cd��CfxRCh��Cj��Cl��Cn��Cp��Cr��Ct��Cv��Cx��Cz��C|��C~��C�H�C�H�C�H�C�H�C�H�C�H�C�H�C�H�C�H�C�H�C�H�C�H�C�H�C�H�C�H�C�H�C�H�C�H�C�H�C�H�C�H�C�H�C�H�C�H�C�H�C�H�C�H�C�H�C�H�C�H�C�H�C�H�C�H�C�H�C�H�C�H�C�H�C�H�C�H�C�H�C�H�C�H�C�H�C�H�C�H�C�H�C�H�C�H�C�H�C�H�C�H�C�H�C�H�C�H�C�H�C�H�C�H�C�H�C�H�C�H�C�H�C�H�C�H�C�H�C�H�C�H�C�H�C�H�C�H�C�H�C�H�C�H�C�H�C�H�C�H�C�<)C�H�C�H�C�H�C�H�C�H�C�H�C�H�C�H�C�H�C�H�C�H�C�H�C�H�C�H�C�H�C�H�C�H�C�H�C�H�C�H�C�H�C�H�C�H�C�H�C�H�C�H�C�H�C�H�C�H�C�H�C�H�C�H�C�H�C�H�C�U�C�H�C�H�C�H�C�H�C�H�C�H�C�H�C�H�C�H�C�H�C�H�C�H�C�H�C�H�C�H�C�H�C�H�D ${D �{D${D�{D${D�{D${D�{D${D�{D${D�{D${D�{D${D�{D${D�{D	${D	�{D
${D
�{D${D�{D${D�{D${D�{D${D�{D${D�{D${D�{D${D�{D${D�{D${D�{D${D�{D${D�{D${D�{D${D�{D${D�{D${D�{D${D�{D${D�{D${D�{D${D�{D${D�{D${D�{D ${D �{D!${D!�{D"${D"�{D#${D#�{D$${D$�D%${D%�{D&${D&�{D'${D'�{D(${D(�{D)${D)�{D*${D*�{D+${D+�{D,${D,�{D-${D-�{D.${D.�{D/${D/�{D0${D0�{D1${D1�{D2${D2�{D3${D3�{D4${D4�{D5${D5�{D6${D6�{D7${D7�{D8${D8�{D9${D9�{D:${D:�{D;${D;�{D<${D<�{D=${D=�{D>${D>�{D?${D?�{D@${D@�{DA${DA�{DB${DB�{DC${DC�{DD${DD�{DE${DE�{DF${DF�{DG${DG�{DH${DH�{DI${DI�{DJ${DJ�{DK${DK�{DL${DL�{DM${DM�{DN${DN�{DO${DO�{DP${DP�{DQ${DQ�{DR${DR�{DS${DS�{DT${DT�{DU${DU�{DV${DV�{DW${DW�{DX${DX�{DY${DY�{DZ${DZ�{D[${D[�{D\${D\�{D]${D]�{D^${D^�{D_${D_�{D`${D`�{Da${Da�{Db${Db�{Dc${Dc�{Dd${Dd�{De${De�{Df${Df�{Dg${Dg�{Dh${Dh�{Di*�Di�{Dj${Dj�{Dk${Dk�{Dl${Dl�{Dm${Dm�{Dn${Dn��Do${Do�{Dp${Dp�{Dq${Dq�{Dr${Dr�{Ds${Ds�{Dt${Dt�{Du${Du�{Dv${Dv�{Dw${Dw�{Dx${Dx�{Dy${Dy�{Dz${Dz�{D{${D{�{D|${D|�{D}${D}�{D~${D~�{D${D�{D�=D�R=D��=D��=D�=D�R=D��=D��=D�=D�R=D��=D��=D�=D�R=D��=D��=D�=D�R=D��=D��=D�=D�R=D��=D��=D�=D�R=D��=D��=D�=D�R=D��=D��=D�=D�R=D��=D��=D�=D�R=D��=D��=D�=D�R=D��=D��=D�=D�R=D��=D��=D�=D�R=D��=D��=D�=D�R=D��=D��=D�=D�R=D��=D��=D�=D�R=D��=D��=D�=D�R=D��=D��=D�=D�R=D��=D��=D�=D�R=D��=D��=D�=D�R=D��=D��=D�=D�R=D��=D��=D�=D�R=D��=D��=D�=D�R=D��=D��=D�=D�R=D��=D��=D�=D�R=D��=D��=D�=D�R=D��=D��=D�=D�R=D��=D��=D�=D�R=D��=D��=D�=D�R=D��qD��=D�=D�R=D��=D��=D�=D�R=D��=D��=D�=D�R=D��=D��=D�=D�R=D��=D��=D�=D�R=D��=D��=D�=D�R=D��=D��=D�=D�R=D��=D��=D�=D�R=D��=D��=D�=D�R=D��=D��=D�=D�R=D��=D��=D�=D�R=D��=D��=D�=D�R=D��=D��=D�=D�R=D��=D��=D�=D�R=D��=D��=D�=D�R=D��=D��=D�=D�R=D��=D��=D�=D�R=D��=D��=D�=D�R=D��=D��=D�=D�R=D��=D��=D�=D�R=D��=D��=D�=D�R=D��=D��=D�=D�R=D��=D��=D�=D�R=D��=D��=D�=D�R=D��=D��=D�=D�R=D��=D��=D�=D�R=D��=D��=D�=D�R=D��=D��=D�=D�R=D��=D��=D�=D�R=D��=D��=D�=D�R=D��=D��=D�=D�R=D��=D��=D�=D�R=D��=D��=D�=D�R=D��=D��=D�=D�R=D��=D��=D�=D�R=D��=D��=D�=D�R=D��=D��=D�=D�R=D��=D��=D�=D�R=D=D��=D�=D�R=DÒ=D��=D�=D�R=DĒ=D��=D�=D�R=DŒ=D��=D�=D�R=Dƒ=D��=D�=D�R=Dǒ=D��=D�=D�R=DȒ=D��=D�=D�R=Dɒ=D��=D�=D�R=Dʒ=D��=D�=D�R=D˒=D��=D�=D�R=D̒=D��=D�=D�R=D͒=D��=D�=D�R=DΒ=D��=D�=D�R=Dϒ=D��=D�=D�R=DВ=D��=D�=D�R=Dђ=D��=D�=D�R=DҒ=D��=D�=D�R=DӒ=D��=D�=D�R=DԒ=D��=D�=D�R=DՒ=D��=D�=D�R=D֒=D��=D�=D�R=Dג=D��=D�=D�R=Dؒ=D��=D�=D�R=Dْ=D��=D�=D�R=Dڒ=D��=D�=D�R=Dے=D��=D�=D�R=Dܒ=D��=D�=D�R=Dݒ=D��=D�=D�R=Dޒ=D��=D�=D�R=Dߒ=D��=D�=D�R=D��=D��=D�=D�R=D�=D��=D�=D�R=D�=D��=D�=D�R=D�=D��=D�=D�R=D�=D��=D�=D�R=D�=D��=D�=D�R=D�=D��=D�=D�R=D�=D��=D�=D�R=D�=D��=D�=D�R=D�=D��=D�=D�R=D�=D��=D�=D�R=D�=D��=D�=D�R=D�=D��=D�=D�R=D�=D��=D�=D�R=D�=D��=D�=D�R=D�=D��=D�=D�R=D�=D��=D�=D�R=D�=D��=D�=D�R=D�=D��=D�=D�R=D�=D��=D�=D�R=D��=D��=D�=D�R=D��=D��=D�=D�R=D��=D��=D�=D�R=D��=D��=D�=D�R=D��=D��=D�=D�R=D��=D��=D�=D�UqD�
11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   A�VA�oA��A��A��A��A��A��A��A��A�&�A�(�A�(�A�"�A��A���AͮA͗�A�t�A�XA�;dAˡ�A�-A�ffA��DA�ZA���A��wA�ƨA�G�A���A���A���A���A�A�A��A��DA�I�A��RA�K�A���A�dZA��A�A� �A�{A���A��A�{A��!A�33A��hA��
A�ffA��RA���A���A��A��RA���A�l�A�(�A�`BA�"�A�$�A���A��hA��A���A��FA��A�jA��A��A�ZA�1A���A�%A��TA��-A���A�?}A��+A���A�t�A�jA�jA��A��A��RA��A��A�A�+A���A��`A���A��FA�p�A�\)A�ffA�A�l�A��A�9XA�9XA�n�A�JA�+A�x�A��wA�O�A�1A��;A�r�A�E�A�oA�A~(�A{Azr�Ax�`Av��Aut�At�yApv�AmC�Ak�-Ah��AfbNAd1'AcdZAc�A`{A]K�A[�^AZjAYp�AX5?AVȴATM�ASAN��AMhsAM?}AL�RAKXAJbAH�/AH  AG��AG�AG�AE�AC%AB~�AA��A@n�AA�PAA�A?��A>^5A=��A<~�A;��A:��A9�mA8ĜA8jA7�wA7�A4�A3�mA3XA2��A2ZA1�hA.�!A-��A,�A,�A,  A*��A)�#A)+A((�A'x�A&=qA%��A%x�A%\)A$�`A#G�A!hsA��A�!AĜAO�A��A�A�wAC�A�+A�PAK�A1'A�hA��A"�AJA�wA�AVA�+A1'A�A{A�AG�Az�A1A`BA/A
��A
�!A
-A��AbNA�A~�A�wAhsA�yAjA�7A�A-A\)A+A I�@��+@��@�^5@�G�@��u@�1'@���@���@�Z@��@�E�@�9@�C�@��@�&�@ꗍ@��`@���@��@���@�\@��/@��@��;@�K�@���@�-@܃@�-@���@�~�@��T@�1'@�M�@�7L@Ο�@�X@��m@�33@�{@�hs@��`@ȣ�@�Q�@��
@Ǯ@�C�@���@��#@��@ēu@�C�@�ȴ@��#@��/@�Z@�ƨ@��@���@�hs@��9@�Z@��F@���@�$�@���@�@���@��`@��@�@��@��@���@��u@�Z@�ƨ@��R@���@��^@�x�@��@��/@��u@�1'@�|�@��H@�v�@�@�`B@��u@�dZ@��@�
=@��\@��@�?}@��j@�r�@�Z@�  @�ȴ@�^5@�=q@�5?@�E�@�5?@�-@�J@�J@�-@���@���@��@���@��@�1'@���@��P@�K�@�+@�
=@���@�V@�{@��^@�X@���@�Ĝ@�j@��m@�S�@�o@�ȴ@��\@�ff@�J@�@�x�@�&�@��@��@��/@�r�@�  @���@�;d@��@�ȴ@�n�@��@���@�`B@�V@��@�j@�9X@��@���@���@���@�|�@�"�@��y@���@��\@�~�@�n�@�5?@��@��@���@��^@��h@�7L@��@��`@��`@�Ĝ@�j@�A�@�1'@�1@��w@�dZ@�S�@�K�@�33@�o@��R@���@�~�@�V@��@�J@��T@��-@���@���@���@��@�%@�%@���@���@��j@��j@�Z@�Q�@�(�@���@�K�@���@�M�@��@�{@�@�J@�J@�{@�-@�ff@�ȴ@�@��@�ff@���@�p�@�&�@�V@��@��j@��D@�r�@�bN@� �@���@���@��m@��
@�ƨ@��
@�ƨ@��;@��P@�t�@���@�(�@� �@��@��@��P@�33@��@�ȴ@��!@��+@�M�@�E�@��@�J@�@��T@���@��-@�x�@�`B@�`B@���@���@���@��9@��D@�bN@�9X@��@�ƨ@���@�t�@�33@��R@�~�@�=q@�-@�{@���@��^@��@�X@���@���@��@�I�@�  @�P@�@~��@~��@~�y@}�@}�h@}?}@}?}@}V@|��@{�m@{��@{33@z�!@z~�@z~�@zn�@z=q@y��@y�^@y��@yG�@xĜ@xQ�@xb@w�@w�w@w��@wl�@v��@v�+@vE�@v$�@v@u@u�@uO�@u?}@u/@uV@t�j@t(�@s�
@st�@so@r�@r��@r^5@r-@q��@p�`@p�@o�w@o
=@n��@nff@n@m�h@m�@l��@l��@l�@lz�@l�@k��@k�
@kdZ@j�!@j~�@jM�@i�#@ihs@h��@h�@h�@hr�@h  @g��@g
=@f5?@e�T@e�@eV@d�j@d9X@cƨ@c��@cdZ@b�@b��@b�\@bM�@b=q@b^5@a�@a&�@`��@`1'@_l�@_+@^�@^@]��@]�@]�@]/@\��@\�/@\�@\�@[33@Z�H@Z��@Z��@ZM�@Y��@Y��@Y�7@Y&�@X��@XQ�@W��@Wl�@W|�@Wl�@V��@V$�@U�-@U/@T��@T�@T(�@S�m@S�@SC�@R�!@Rn�@Q��@Q�^@Qhs@P��@P  @Ol�@N��@NV@N$�@M@L��@L�@Lj@L9X@K��@KdZ@K"�@J�!@Jn�@JM�@JJ@I��@I%@H��@H��@H�u@Hr�@HA�@G�;@GK�@F��@F�@F��@Fv�@F$�@E�-@EO�@D��@D��@D�@D�D@Dj@D9X@C��@Cƨ@Ct�@CS�@C33@Co@B�H@B��@B~�@B^5@B-@A�#@Ahs@@��@@Ĝ@@�9@@r�@@1'@?�@?�;@?��@?��@?�P@?K�@?�@>ȴ@>V@>{@>@=@=`B@=/@<��@<�@<I�@<(�@;��@;C�@;"�@:��@:n�@:-@:J@:J@9��@9�@9��@9��@9x�@9�@9%@8�`@8�@8 �@7�;@7�w@7��@7�P@7\)@6�+@6{@6{@5��@5@5@5@5@5?}@4��@4��@4��@4�@3�m@3�F@333@3"�@2��@2�\@2=q@1�@1��@1%@0�u@0A�@/�;@/��@/�@.�R@.V@.{@-�-@-p�@-?}@-V@,j@+ƨ@+t�@+C�@*��@*n�@)��@)hs@)%@(Q�@'��@';d@&��@&�+@&5?@&@%�-@%�@%�@%`B@%/@$�/@$��@$j@$I�@$9X@$�@$1@#��@#��@#@"n�@!�@!��@!�7@!x�@!hs@!&�@ Ĝ@ �u@ r�@ Q�@ A�@ 1'@�@�w@�w@�w@|�@;d@�@��@�y@�y@�y@��@��@ff@$�@$�@�h@V@�@z�@j@(�@�@��@��@dZ@dZ@"�@��@�!@��@�\@M�@-@�@-@�@�@��@�@�#@�^@x�@&�@��@��@Ĝ@�9@�@Q�@1'@b@�;@��@+@
=@��@��@��@�y@�@�R@��@E�@��@p�@`B@`B@`B@`B@`B@`B@O�@�@�/@��@�D@�D@z�@Z@I�@�@1@�m@�
@ƨ@�F@��@�@t�@t�@C�@"�@o@��@�!@�!@��@�\@M�@��@��@�^@��@hs@G�@7L@�@��@�9@�u@�u@�u@�u@�@�@�@r�@bN@ �@�@�;@��@�@�P@|�@K�@;d@�@
=@ȴ@��@v�@ff@V@E�@E�@5?@�@�h@�@��@�@�@�/@�j@j@z�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   A�VA�oA��A��A��A��A��A��A��A��A�&�A�(�A�(�A�"�A��A���AͮA͗�A�t�A�XA�;dAˡ�A�-A�ffA��DA�ZA���A��wA�ƨA�G�A���A���A���A���A�A�A��A��DA�I�A��RA�K�A���A�dZA��A�A� �A�{A���A��A�{A��!A�33A��hA��
A�ffA��RA���A���A��A��RA���A�l�A�(�A�`BA�"�A�$�A���A��hA��A���A��FA��A�jA��A��A�ZA�1A���A�%A��TA��-A���A�?}A��+A���A�t�A�jA�jA��A��A��RA��A��A�A�+A���A��`A���A��FA�p�A�\)A�ffA�A�l�A��A�9XA�9XA�n�A�JA�+A�x�A��wA�O�A�1A��;A�r�A�E�A�oA�A~(�A{Azr�Ax�`Av��Aut�At�yApv�AmC�Ak�-Ah��AfbNAd1'AcdZAc�A`{A]K�A[�^AZjAYp�AX5?AVȴATM�ASAN��AMhsAM?}AL�RAKXAJbAH�/AH  AG��AG�AG�AE�AC%AB~�AA��A@n�AA�PAA�A?��A>^5A=��A<~�A;��A:��A9�mA8ĜA8jA7�wA7�A4�A3�mA3XA2��A2ZA1�hA.�!A-��A,�A,�A,  A*��A)�#A)+A((�A'x�A&=qA%��A%x�A%\)A$�`A#G�A!hsA��A�!AĜAO�A��A�A�wAC�A�+A�PAK�A1'A�hA��A"�AJA�wA�AVA�+A1'A�A{A�AG�Az�A1A`BA/A
��A
�!A
-A��AbNA�A~�A�wAhsA�yAjA�7A�A-A\)A+A I�@��+@��@�^5@�G�@��u@�1'@���@���@�Z@��@�E�@�9@�C�@��@�&�@ꗍ@��`@���@��@���@�\@��/@��@��;@�K�@���@�-@܃@�-@���@�~�@��T@�1'@�M�@�7L@Ο�@�X@��m@�33@�{@�hs@��`@ȣ�@�Q�@��
@Ǯ@�C�@���@��#@��@ēu@�C�@�ȴ@��#@��/@�Z@�ƨ@��@���@�hs@��9@�Z@��F@���@�$�@���@�@���@��`@��@�@��@��@���@��u@�Z@�ƨ@��R@���@��^@�x�@��@��/@��u@�1'@�|�@��H@�v�@�@�`B@��u@�dZ@��@�
=@��\@��@�?}@��j@�r�@�Z@�  @�ȴ@�^5@�=q@�5?@�E�@�5?@�-@�J@�J@�-@���@���@��@���@��@�1'@���@��P@�K�@�+@�
=@���@�V@�{@��^@�X@���@�Ĝ@�j@��m@�S�@�o@�ȴ@��\@�ff@�J@�@�x�@�&�@��@��@��/@�r�@�  @���@�;d@��@�ȴ@�n�@��@���@�`B@�V@��@�j@�9X@��@���@���@���@�|�@�"�@��y@���@��\@�~�@�n�@�5?@��@��@���@��^@��h@�7L@��@��`@��`@�Ĝ@�j@�A�@�1'@�1@��w@�dZ@�S�@�K�@�33@�o@��R@���@�~�@�V@��@�J@��T@��-@���@���@���@��@�%@�%@���@���@��j@��j@�Z@�Q�@�(�@���@�K�@���@�M�@��@�{@�@�J@�J@�{@�-@�ff@�ȴ@�@��@�ff@���@�p�@�&�@�V@��@��j@��D@�r�@�bN@� �@���@���@��m@��
@�ƨ@��
@�ƨ@��;@��P@�t�@���@�(�@� �@��@��@��P@�33@��@�ȴ@��!@��+@�M�@�E�@��@�J@�@��T@���@��-@�x�@�`B@�`B@���@���@���@��9@��D@�bN@�9X@��@�ƨ@���@�t�@�33@��R@�~�@�=q@�-@�{@���@��^@��@�X@���@���@��@�I�@�  @�P@�@~��@~��@~�y@}�@}�h@}?}@}?}@}V@|��@{�m@{��@{33@z�!@z~�@z~�@zn�@z=q@y��@y�^@y��@yG�@xĜ@xQ�@xb@w�@w�w@w��@wl�@v��@v�+@vE�@v$�@v@u@u�@uO�@u?}@u/@uV@t�j@t(�@s�
@st�@so@r�@r��@r^5@r-@q��@p�`@p�@o�w@o
=@n��@nff@n@m�h@m�@l��@l��@l�@lz�@l�@k��@k�
@kdZ@j�!@j~�@jM�@i�#@ihs@h��@h�@h�@hr�@h  @g��@g
=@f5?@e�T@e�@eV@d�j@d9X@cƨ@c��@cdZ@b�@b��@b�\@bM�@b=q@b^5@a�@a&�@`��@`1'@_l�@_+@^�@^@]��@]�@]�@]/@\��@\�/@\�@\�@[33@Z�H@Z��@Z��@ZM�@Y��@Y��@Y�7@Y&�@X��@XQ�@W��@Wl�@W|�@Wl�@V��@V$�@U�-@U/@T��@T�@T(�@S�m@S�@SC�@R�!@Rn�@Q��@Q�^@Qhs@P��@P  @Ol�@N��@NV@N$�@M@L��@L�@Lj@L9X@K��@KdZ@K"�@J�!@Jn�@JM�@JJ@I��@I%@H��@H��@H�u@Hr�@HA�@G�;@GK�@F��@F�@F��@Fv�@F$�@E�-@EO�@D��@D��@D�@D�D@Dj@D9X@C��@Cƨ@Ct�@CS�@C33@Co@B�H@B��@B~�@B^5@B-@A�#@Ahs@@��@@Ĝ@@�9@@r�@@1'@?�@?�;@?��@?��@?�P@?K�@?�@>ȴ@>V@>{@>@=@=`B@=/@<��@<�@<I�@<(�@;��@;C�@;"�@:��@:n�@:-@:J@:J@9��@9�@9��@9��@9x�@9�@9%@8�`@8�@8 �@7�;@7�w@7��@7�P@7\)@6�+@6{@6{@5��@5@5@5@5@5?}@4��@4��@4��@4�@3�m@3�F@333@3"�@2��@2�\@2=q@1�@1��@1%@0�u@0A�@/�;@/��@/�@.�R@.V@.{@-�-@-p�@-?}@-V@,j@+ƨ@+t�@+C�@*��@*n�@)��@)hs@)%@(Q�@'��@';d@&��@&�+@&5?@&@%�-@%�@%�@%`B@%/@$�/@$��@$j@$I�@$9X@$�@$1@#��@#��@#@"n�@!�@!��@!�7@!x�@!hs@!&�@ Ĝ@ �u@ r�@ Q�@ A�@ 1'@�@�w@�w@�w@|�@;d@�@��@�y@�y@�y@��@��@ff@$�@$�@�h@V@�@z�@j@(�@�@��@��@dZ@dZ@"�@��@�!@��@�\@M�@-@�@-@�@�@��@�@�#@�^@x�@&�@��@��@Ĝ@�9@�@Q�@1'@b@�;@��@+@
=@��@��@��@�y@�@�R@��@E�@��@p�@`B@`B@`B@`B@`B@`B@O�@�@�/@��@�D@�D@z�@Z@I�@�@1@�m@�
@ƨ@�F@��@�@t�@t�@C�@"�@o@��@�!@�!@��@�\@M�@��@��@�^@��@hs@G�@7L@�@��@�9@�u@�u@�u@�u@�@�@�@r�@bN@ �@�@�;@��@�@�P@|�@K�@;d@�@
=@ȴ@��@v�@ff@V@E�@E�@5?@�@�h@�@��@�@�@�/@�j@j@z�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   Be`Be`Be`Be`BdZBdZBdZBdZBdZBdZBe`Be`Be`BdZBaHB_;B]/B[#B\)B]/B_;Bq�B��B�NB��BB%B�B'�B,B0!B9XB9XB:^B?}BL�BhsBhsBl�Bo�Bv�Bv�B�B�B�B}�Bs�Bs�Bq�Bs�Bq�Bn�Bm�Bk�BbNBjBr�Bx�Bt�Bt�Bt�Bq�Bn�BXB?}B:^B9XB<jB7LB6FB7LB?}B:^B9XB=qB?}BA�BK�BT�BffB\)BK�BC�B:^B5?B>wBF�BJ�BC�B8RB �B��B��B�BuBB��B�sBǮB�B�%BH�BPB
�ZB
��B
�wB
��B
�B
��B
��B
��B
�qB
��B
�B
e`B
P�B
0!B
oB
1B	�B	�#B	ȴB	�B	��B	�B	�=B	ffB	`BB	B�B	-B	 �B	7LB	8RB	&�B	{B	JB��B	JB	DB	
=B��B�yB��B�dB�XB�LB�B��B��B��B��B�B�B��B�VB�oB�{B�7B��B�B��B��B��B�\B�7B�B{�Bv�Bv�Bs�Bs�Bs�Bq�Bq�Bo�Bm�BjB`BBVBT�BYBT�BM�BF�BB�B;dB7LB8RB<jB=qB>wBG�BB�B=qB7LB33B/B+B+B+B+B.B1'B2-B1'B1'B-B,B,B.B/B0!B0!B1'B0!B/B.B1'B/B.B-B-B,B+B+B,B(�B(�B'�B'�B%�B$�B%�B"�B"�B �B �B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B#�B#�B$�B%�B'�B'�B(�B,B-B,B-B-B/B0!B0!B49B49B7LB8RB:^B:^B;dB<jB=qB=qB=qB?}BA�BB�BA�BB�BD�BD�BG�BG�BH�BJ�BK�BN�BP�BQ�BQ�BQ�BR�BR�BR�BQ�BR�BT�BXB\)BaHBgmBl�Bp�Br�Bw�B~�B�B�7B�hB��B��B��B��B��B��B�B�B�B�B�'B�?B�FB�^B�jB�}B��BŢBƨBȴB��B��B��B��B��B��B�B�B�/B�;B�BB�;B�BB�HB�fB�yB�B�B�B�B�B��B��B��B��B��B��B	  B	B		7B	DB	VB	bB	oB	{B	�B	�B	�B	!�B	#�B	$�B	%�B	+B	.B	0!B	49B	6FB	9XB	=qB	A�B	D�B	H�B	K�B	P�B	S�B	VB	XB	YB	ZB	[#B	]/B	aHB	dZB	ffB	iyB	jB	k�B	o�B	s�B	s�B	u�B	v�B	w�B	z�B	z�B	}�B	� B	�B	�+B	�1B	�7B	�=B	�JB	�VB	�\B	�bB	�bB	�uB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�B	�B	�!B	�'B	�3B	�9B	�?B	�FB	�LB	�XB	�jB	��B	��B	��B	��B	�}B	�}B	�}B	�}B	�}B	�}B	��B	��B	B	B	ĜB	ĜB	ĜB	ŢB	ǮB	ȴB	ɺB	ɺB	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�
B	�B	�B	�B	�B	�B	�B	�B	�#B	�)B	�)B	�)B	�/B	�/B	�/B	�5B	�;B	�;B	�BB	�HB	�HB	�HB	�NB	�TB	�ZB	�`B	�fB	�mB	�sB	�yB	�yB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
  B
  B
  B
  B
B
B
B
B
B
B
B
B
B
B
B
B
B
%B
+B
+B
+B
1B
	7B
	7B

=B

=B

=B

=B
DB
DB
DB
DB
JB
PB
PB
VB
\B
bB
hB
hB
hB
hB
oB
oB
uB
uB
uB
{B
{B
{B
�B
�B
{B
{B
{B
{B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
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
�B
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
!�B
!�B
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
&�B
&�B
'�B
(�B
(�B
(�B
(�B
)�B
)�B
+B
+B
+B
+B
+B
,B
,B
,B
,B
-B
-B
-B
-B
.B
/B
/B
/B
/B
/B
/B
/B
/B
/B
/B
/B
0!B
0!B
0!B
0!B
1'B
1'B
2-B
2-B
2-B
2-B
2-B
33B
49B
49B
49B
49B
49B
5?B
5?B
5?B
6FB
6FB
6FB
6FB
7LB
7LB
7LB
8RB
8RB
8RB
9XB
:^B
:^B
;dB
;dB
;dB
;dB
;dB
;dB
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
>wB
>wB
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
?}B
@�B
?}B
?}B
@�B
@�B
@�B
A�B
B�B
C�B
C�B
B�B
B�B
C�B
D�B
D�B
E�B
E�B
E�B
F�B
F�B
G�B
G�B
H�B
H�B
H�B
H�B
H�B
I�B
I�B
I�B
J�B
J�B
J�B
J�B
J�B
K�B
K�B
L�B
L�B
L�B
L�B
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
N�B
N�B
N�B
N�B
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
S�B
S�B
S�B
T�B
T�B
T�B
T�B
T�B
T�B
VB
VB
VB
W
B
XB
XB
YB
XB
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
\)B
\)B
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
]/B
^5B
^5B
^5B
^5B
^5B
^5B
_;B
_;B
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
aHB
aHB
aHB
bNB
bNB
bNB
bNB
cTB
cTB
cTB
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
e`B
ffB
ffB
ffB
ffB
gmB
gmB
gmB
gmB
gmB
gmB
gmB
gmB
gmB
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
hsB
hsB
iyB
iyB
iyB
iyB
iyB
iyB
iyB
iyB
iyB
jB
jB
jB
jB
jB
jB
jB
k�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   BeFBe,Be,Be,Bd&Bd&Bd&Bd&Bd&Bd&Be,Be,Be,Bd&BaB_B\�B[	B[�B\�B_BqvBΥB�B��B�B�BeB'�B+�B/�B9$B9$B:*B?HBL�Bh>Bh>BlWBoiBv�Bv�B��B��B��B}�Bs�Bs�BqvBs�BqvBncBm]BkQBbBjKBr|Bx�Bt�Bt�Bt�BqvBncBW�B?HB:*B9$B<6B7B6B7B?HB:*B9$B=<B?HBAUBK�BT�Bf2B[�BK�BCaB:*B5B>BBFtBJ�BCaB8B �B��B��BkB@B�B��B�>B�zB��B��BH�BB
�&B
ϫB
�BB
��B
��B
��B
�YB
��B
�<B
��B
��B
e,B
P�B
/�B
:B
�B	�]B	��B	ȀB	��B	��B	��B	��B	f2B	_�B	B[B	,�B	 vB	7B	8B	&�B	FB	�B��B	B	
�B	
	B��B�DB˒B�0B�$B�B��B��B��B��B��B��B��B��B�B� B�FB��B�qB��B��B�?B�YB�(B�B��B{�BvzBv�Bs�Bs�BshBqvBq[BoOBmCBjKB_�BU�BT�BX�BT�BM�BFtBBAB;B6�B8B<6B=<B>BBGzBB[B="B7B2�B.�B*�B*�B*�B*�B-�B0�B1�B0�B0�B,�B+�B+�B-�B.�B/�B/�B0�B/�B.�B-�B0�B.�B-�B,�B,�B+�B*�B*�B+�B(�B(�B'�B'�B%�B$�B%�B"�B"�B vB �B�BdBjB]BqBxBkBWBdBjB~BdB]B~BjBjB�BdB~BKBeBkBjB#�B#�B$�B%�B'�B'�B(�B+�B,�B+�B,�B,�B.�B/�B/�B3�B4B7B8B:*B:B;B<B="B=<B=<B?.BAUBBABA;BB[BDMBDgBG_BG_BH�BJ�BKxBN�BP�BQ�BQ�BQ�BR�BR�BR�BQ�BR�BT�BW�B[�B`�BgBlWBpUBraBw�B~�B��B�B�B�EB�QB�dB�vB��B��B��B��B��B��B��B�B��B�*B�B�.B�;B�SB�YB�fB�~B̈́B͟BϑBңBԯBյB��B��B��B��B��B��B��B�B�*B�6B�CB�oB�[B�aB�nB��B��B��B��B��B��B	�B	�B	
�B	B	.B	 B	,B	?B	QB	dB	!|B	#�B	$�B	%�B	*�B	-�B	/�B	3�B	5�B	9$B	="B	A;B	DMB	HfB	KxB	P�B	S�B	U�B	W�B	X�B	Y�B	Z�B	\�B	`�B	dB	fB	i*B	jKB	k6B	oOB	shB	shB	utB	vzB	w�B	z�B	z�B	}�B	�B	��B	��B	��B	�B	��B	��B	�B	�B	�.B	�.B	�&B	�9B	�9B	�?B	�EB	�WB	�xB	�jB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	��B	��B	�	B	�B	�4B	�4B	�;B	�4B	�.B	�.B	�.B	�.B	�HB	�.B	�4B	�;B	�[B	�AB	�MB	�MB	�MB	�SB	�zB	�fB	ɆB	ɆB	�rB	ΥB	ѝB	ѝB	ѝB	ңB	өB	յB	յB	յB	յB	ּB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	��B	�B	�B	�B	�B	�8B	�$B	�DB	�*B	�0B	�0B	�6B	�=B	�CB	�CB	�CB	�IB	�IB	�OB	�OB	�OB	�UB	�UB	�[B	�[B	�[B	�|B	�hB	�B	�B	�tB	�tB	�tB	�tB	�tB	�tB	�zB	��B	�zB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
 �B
 �B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
	B
	�B
	�B

	B

	B
B

�B

�B

�B
�B
B
B
B
B
B
4B
B
4B
B
 B
 B
@B
&B
&B
FB
,B
,B
2B
2B
FB
,B
,B
,B
2B
2B
9B
?B
?B
?B
?B
?B
?B
EB
YB
YB
?B
YB
EB
eB
KB
KB
QB
WB
QB
QB
QB
QB
qB
WB
WB
WB
WB
WB
QB
WB
]B
]B
]B
]B
xB
dB
jB
jB
jB
jB
�B
jB
pB
�B
 vB
 vB
 vB
!�B
!|B
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
&�B
&�B
'�B
(�B
(�B
(�B
(�B
)�B
)�B
*�B
*�B
*�B
*�B
*�B
+�B
+�B
+�B
+�B
,�B
,�B
,�B
,�B
-�B
.�B
.�B
.�B
.�B
.�B
.�B
.�B
.�B
.�B
.�B
.�B
/�B
/�B
/�B
/�B
0�B
0�B
1�B
1�B
1�B
1�B
1�B
2�B
3�B
3�B
3�B
3�B
3�B
4�B
4�B
4�B
6B
5�B
5�B
6B
7B
7B
7B
8B
8B
8B
9	B
:B
:B
;B
;B
;0B
;B
;0B
;B
<B
<B
<6B
<6B
=<B
="B
="B
=<B
="B
="B
>(B
>(B
>(B
>BB
>(B
>(B
>(B
>BB
?.B
?HB
?.B
?.B
?.B
@OB
?HB
?HB
@4B
@4B
@4B
A;B
BAB
CGB
CGB
BAB
BAB
CaB
DMB
DMB
ESB
ESB
ESB
FtB
FYB
G_B
GzB
H�B
HfB
HfB
HfB
HfB
IlB
IlB
IlB
JrB
JrB
JrB
J�B
JrB
K�B
KxB
L�B
L~B
L~B
L~B
L~B
M�B
M�B
M�B
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
S�B
S�B
S�B
T�B
T�B
T�B
T�B
T�B
T�B
U�B
U�B
U�B
V�B
W�B
W�B
X�B
W�B
X�B
X�B
X�B
Y�B
Y�B
Y�B
Y�B
Y�B
Y�B
Y�B
Y�B
Z�B
[�B
[�B
[�B
[�B
[�B
[�B
[�B
[�B
[�B
[�B
\�B
\�B
\�B
]�B
]�B
]�B
]�B
]�B
]�B
^�B
^�B
^�B
^�B
^�B
_�B
_�B
_�B
_�B
`B
_�B
_�B
`�B
`�B
`�B
`�B
`�B
aB
`�B
`�B
`�B
`�B
a�B
a�B
a�B
a�B
cB
cB
cB
cB
cB
cB
cB
c B
cB
dB
dB
d&B
dB
d&B
dB
dB
dB
d&B
dB
e,B
eB
eB
e,B
eB
eB
e,B
e,B
eB
eB
f2B
fB
f2B
fB
g8B
gB
gB
gB
gB
gB
gB
gB
g8B
gB
gB
gB
gB
g8B
gB
h$B
h$B
h$B
h>B
h$B
h>B
h$B
h$B
i*B
i*B
i*B
iDB
i*B
i*B
i*B
i*B
i*B
j0B
j0B
j0B
j0B
jKB
j0B
j0B
k611111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   @��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
PRES            TEMP            PSAL            PRES            TEMP            PSAL            PRES_ADJ=PRES-SP,  where SP is SURFACE PRESSURE from next cycle; PRES_ADJ_ERR=Manufacturer sensor accuracy                                                                                                                                                      TEMP_ADJ=TEMP; TEMP_ADJ_ERR=Manufacturer sensor accuracy                                                                                                                                                                                                        PSAL_ADJ = RecalS= psal(PRES_ADJ,TEMP,Conductivity)                                                                                                                                                                                                             None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            PSAL_ADJ_ERR=max(RecalS error, 0.01(PSS-78))                                                                                                                                                                                                                    SP=-0.57(dbar)                                                                                                                                                                                                                                                  None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            Pressure Correction using reported SURFACE PRESSURE                                                                                                                                                                                                             None                                                                                                                                                                                                                                                            Salinity Recalculation using PRES_ADJ                                                                                                                                                                                                                           None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            No adjustment is needed                                                                                                                                                                                                                                         201808170044062018081700440620180817004406201808180037532018081800375320180818003753JA  ARFMdecpA19c                                                                20180812003510  IP                  G�O�G�O�G�O�                JA  ARFMfmtp2.8                                                                 20180811153519  IP                  G�O�G�O�G�O�                JA  ARCAsspa3.1b                                                                20180811153520  IP  PRES            G�O�G�O�G�O�                JA  ARGQrqcppo_c                                                                20180811153520  QCP$                G�O�G�O�G�O�              3CJA  ARGQrqcpt19d                                                                20180811153521  QCP$                G�O�G�O�G�O�           80000JA  ARGQaqcpt19d                                                                20180811153521  QCP$                G�O�G�O�G�O�           80000JA  ARGQrqcp2.8e                                                                20180811153521  QCP$                G�O�G�O�G�O�            FB40JA  ARGQaqcp2.8e                                                                20180811153521  QCP$                G�O�G�O�G�O�            FB40JA  ARGQrqcpt16c                                                                20180811153521  QCP$                G�O�G�O�G�O�           10000JA      jafc1.0                                                                 20180811153522                      G�O�G�O�G�O�                JA  ARUP                                                                        20180811155523                      G�O�G�O�G�O�                JM  ARGQJMQC2.0                                                                 20180811153831  CV  JULD            G�O�G�O�F���                JM  ARCAJMQC2.0                                                                 20180816154406  CV  PRES_ADJUSTED   G�O�G�O�G�O�                JM  ARCAJMQC2.0                                                                 20180816154406  CV  PSAL_ADJUSTED   G�O�G�O�G�O�                JM  ARSQOW  1.1 2017V1                                                          20180817153753  IP  PSAL_ADJUSTED   G�O�G�O�G�O�                JA  ARDU                                                                        20200120021521                      G�O�G�O�G�O�                