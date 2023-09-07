CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             
   title         Argo float vertical profile    institution       JMA    source        
Argo float     history       b2019-11-24T06:37:14Z creation;2019-11-24T06:37:20Z conversion to V3.1;2023-06-29T05:50:37Z update;     
references        (http://www.argodatamgt.org/Documentation   comment       	free text      user_manual_version       3.1    Conventions       Argo-3.1 CF-1.6    featureType       trajectoryProfile      comment_dmqc_operator         BPRIMARY|https://orcid.org/0000-0001-9150-6442|Kanako Sato, JAMSTEC        @   	DATA_TYPE                  	long_name         	Data type      conventions       Argo reference table 1     
_FillValue                    7,   FORMAT_VERSION                 	long_name         File format version    
_FillValue                    7<   HANDBOOK_VERSION               	long_name         Data handbook version      
_FillValue                    7@   REFERENCE_DATE_TIME                 	long_name         !Date of reference for Julian days      conventions       YYYYMMDDHHMISS     
_FillValue                    7D   DATE_CREATION                   	long_name         Date of file creation      conventions       YYYYMMDDHHMISS     
_FillValue                    7T   DATE_UPDATE                 	long_name         Date of update of this file    conventions       YYYYMMDDHHMISS     
_FillValue                    7d   PLATFORM_NUMBER                   	long_name         Float unique identifier    conventions       WMO float identifier : A9IIIII     
_FillValue                    7t   PROJECT_NAME                  	long_name         Name of the project    
_FillValue                  @  7|   PI_NAME                   	long_name         "Name of the principal investigator     
_FillValue                  @  7�   STATION_PARAMETERS           	            	long_name         ,List of available parameters for the station   conventions       Argo reference table 3     
_FillValue                  0  7�   CYCLE_NUMBER               	long_name         Float cycle number     conventions       =0...N, 0 : launch cycle (if exists), 1 : first complete cycle      
_FillValue         ��        8,   	DIRECTION                  	long_name         !Direction of the station profiles      conventions       -A: ascending profiles, D: descending profiles      
_FillValue                    80   DATA_CENTRE                   	long_name         .Data centre in charge of float data processing     conventions       Argo reference table 4     
_FillValue                    84   DC_REFERENCE                  	long_name         (Station unique identifier in data centre   conventions       Data centre convention     
_FillValue                     88   DATA_STATE_INDICATOR                  	long_name         1Degree of processing the data have passed through      conventions       Argo reference table 6     
_FillValue                    8X   	DATA_MODE                  	long_name         Delayed mode or real time data     conventions       >R : real time; D : delayed mode; A : real time with adjustment     
_FillValue                    8\   PLATFORM_TYPE                     	long_name         Type of float      conventions       Argo reference table 23    
_FillValue                     8`   FLOAT_SERIAL_NO                   	long_name         Serial number of the float     
_FillValue                     8�   FIRMWARE_VERSION                  	long_name         Instrument firmware version    
_FillValue                     8�   WMO_INST_TYPE                     	long_name         Coded instrument type      conventions       Argo reference table 8     
_FillValue                    8�   JULD               standard_name         time   	long_name         ?Julian day (UTC) of the station relative to REFERENCE_DATE_TIME    units         "days since 1950-01-01 00:00:00 UTC     conventions       8Relative julian days with decimal part (as parts of day)   
resolution        >�����h�   
_FillValue        A.�~       axis      T           8�   JULD_QC                	long_name         Quality on date and time   conventions       Argo reference table 2     
_FillValue                    8�   JULD_LOCATION                  	long_name         @Julian day (UTC) of the location relative to REFERENCE_DATE_TIME   units         "days since 1950-01-01 00:00:00 UTC     conventions       8Relative julian days with decimal part (as parts of day)   
resolution        >�����h�   
_FillValue        A.�~            8�   LATITUDE               	long_name         &Latitude of the station, best estimate     standard_name         latitude   units         degree_north   
_FillValue        @�i�       	valid_min         �V�        	valid_max         @V�        axis      Y           8�   	LONGITUDE                  	long_name         'Longitude of the station, best estimate    standard_name         	longitude      units         degree_east    
_FillValue        @�i�       	valid_min         �f�        	valid_max         @f�        axis      X           8�   POSITION_QC                	long_name         ,Quality on position (latitude and longitude)   conventions       Argo reference table 2     
_FillValue                    8�   POSITIONING_SYSTEM                    	long_name         Positioning system     
_FillValue                    8�   PROFILE_PRES_QC                	long_name         #Global quality flag of PRES profile    conventions       Argo reference table 2a    
_FillValue                    8�   PROFILE_TEMP_QC                	long_name         #Global quality flag of TEMP profile    conventions       Argo reference table 2a    
_FillValue                    8�   PROFILE_PSAL_QC                	long_name         #Global quality flag of PSAL profile    conventions       Argo reference table 2a    
_FillValue                    8�   VERTICAL_SAMPLING_SCHEME                  	long_name         Vertical sampling scheme   conventions       Argo reference table 16    
_FillValue                    9    CONFIG_MISSION_NUMBER                  	long_name         :Unique number denoting the missions performed by the float     conventions       !1...N, 1 : first complete mission      
_FillValue         ��        :    PRES         
      
   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���   axis      Z        �  :   PRES_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  I�   PRES_ADJUSTED            
      	   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  Ml   PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  \�   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     
_FillValue        G�O�   	valid_min         �      	valid_max         B      C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  `�   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  pX   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     
_FillValue        G�O�   	valid_min         �      	valid_max         B      C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  t<   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ��   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    
_FillValue        G�O�   	valid_min         @      	valid_max         B$     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  ��   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �(   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    
_FillValue        G�O�   	valid_min         @      	valid_max         B$     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  �   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ��   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         decibar    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  �t   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         degree_Celsius     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  ��   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  �|   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  �  �    SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                 	   ݐ   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                 	   �   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                 	   �   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  �  ��   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                    �   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                    �   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                    �   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                    �   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  @  �    HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                    �`   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                    �p   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    �t   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar         ��   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar         ��   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�        ��   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    ��Argo profile    3.1 1.2 19500101000000  20191124063714  20230705031506  5905058                                                                 JAMSTEC                                                         PRES            TEMP            PSAL               �A   JA  I2_0675_191                     2C  D   NAVIS_A                         0675                            ARGO 102115                     863 @��G���1   @��H��-�@7kxF�]�b�I�^51   GPS     A   A   A   Primary sampling: averaged [bin average for 1 Hz CTD]                                                                                                                                                                                                              @�  @�  A   A   A@  A`  A�  A�33A�  A�  A�  A�  A�33A�  B   B  B  B  B   B(  B0  B8  B@  BH  BP  BX  B`  Bh  Bp  Bx  B�  B�  B�  B�  B�  B�  B�33B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  C   C  C  C  C  C
  C  C  C  C  C�fC  C  C  C  C  C   C"  C$  C&  C(  C)�fC,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DPy�DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dt� Du  Du� Dv  Dv� Dw  Dw� Dx  Dx� Dy  Dy� Dz  Dz� D{  D{� D|  D|� D}  D}� D~  D~� D  D� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�C3D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D D�� D�  D�@ DÀ D�� D�  D�@ DĀ D�� D�  D�@ Dŀ D�� D�  D�@ Dƀ D�� D�  D�@ Dǀ D�� D�  D�@ DȀ D�� D�  D�@ Dɀ D�� D�  D�@ Dʀ D�� D�  D�@ Dˀ D�� D�  D�@ D̀ D�� D�  D�@ D̀ D�� D�  D�C3D΀ D�� D�  D�@ Dπ D�� D�  D�@ DЀ D�� D�  D�@ Dр D�� D�  D�@ DҀ D�� D�  D�@ DӀ D�� D�  D�@ DԀ D�� D�  D�@ DՀ D�� D�  D�@ Dր D�� D�  D�<�D׀ D�� D�  D�@ D؀ D�� D�  D�@ Dـ D�� D�  D�@ Dڀ D�� D�  D�@ Dۀ D�� D�  D�@ D܀ D�� D�  D�@ D݀ D�� D�  D�@ Dހ D�� D�  D�@ D߀ D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�C3D�3D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�FfD�c3111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   @�{@�{A
=A+
=AK
=Ak
=A��A��RA��A��AŅAՅA�RA��BB
BBB"B*B2B:BBBJBRBZBbBjBrBzB�aHB�aHB�aHB�aHB�aHB�aHB��{B�aHB�aHB�aHB�aHB�aHB�aHB�aHB�aHB�aHB�aHB�aHB�aHB�aHB�aHB�aHB�aHB�aHB�aHB�aHB�aHB�aHB�aHB�aHB�aHB�aHC ��C��C��C��C��C
��C��C��C��C��C�
C��C��C��C��C��C ��C"��C$��C&��C(��C*�
C,��C.��C0��C2��C4��C6��C8��C:��C<��C>��C@��CB��CD��CF��CH��CJ��CL��CN��CP��CR��CT��CV��CX��CZ��C\��C^��C`��Cb��Cd��Cf��Ch��Cj��Cl��Cn��Cp��Cr��Ct��Cv��Cx��Cz��C|��C~��C�XRC�XRC�XRC�XRC�XRC�XRC�XRC�XRC�XRC�XRC�XRC�eC�XRC�XRC�XRC�XRC�XRC�XRC�XRC�XRC�XRC�XRC�XRC�XRC�XRC�XRC�XRC�XRC�XRC�XRC�XRC�XRC�XRC�XRC�XRC�XRC�XRC�XRC�XRC�XRC�XRC�XRC�XRC�XRC�XRC�XRC�XRC�XRC�XRC�XRC�XRC�XRC�XRC�XRC�XRC�XRC�XRC�XRC�XRC�XRC�XRC�XRC�XRC�XRC�XRC�XRC�XRC�XRC�XRC�XRC�XRC�XRC�XRC�XRC�XRC�XRC�XRC�XRC�XRC�XRC�XRC�XRC�XRC�XRC�XRC�XRC�XRC�XRC�XRC�XRC�XRC�XRC�XRC�XRC�XRC�XRC�XRC�XRC�XRC�XRC�XRC�XRC�XRC�XRC�XRC�XRC�XRC�XRC�XRC�XRC�XRC�XRC�XRC�XRC�XRC�XRC�XRC�XRC�XRC�XRC�XRC�XRC�XRC�XRC�XRC�XRC�XRC�XRD ,)D �)D,)D�)D,)D�)D,)D�)D,)D�)D,)D�)D,)D�)D,)D�)D,)D�)D	,)D	�)D
,)D
�)D,)D�)D,)D�)D,)D�)D,)D�)D,)D�)D,)D�)D,)D�)D,)D�)D,)D�)D,)D�)D,)D�)D,)D�)D,)D�)D,)D�)D,)D�)D,)D�)D,)D�)D,)D�)D,)D�)D,)D�)D,)D�)D ,)D �)D!,)D!�)D",)D"�)D#,)D#�)D$,)D$�)D%,)D%�)D&,)D&�)D',)D'�)D(,)D(�)D),)D)�)D*,)D*�)D+,)D+�)D,,)D,�)D-,)D-�)D.,)D.�)D/,)D/�)D0,)D0�)D1,)D1�)D2,)D2�)D3,)D3�)D4,)D4�)D5,)D5�)D6,)D6�)D7,)D7�)D8,)D8�)D9,)D9�)D:,)D:�)D;,)D;�)D<,)D<�)D=,)D=�)D>,)D>�)D?,)D?�)D@,)D@�)DA,)DA�)DB,)DB�)DC,)DC�)DD,)DD�)DE,)DE�)DF,)DF�)DG,)DG�)DH,)DH�)DI,)DI�)DJ,)DJ�)DK,)DK�)DL,)DL�)DM,)DM�)DN,)DN�)DO,)DO�)DP,)DP��DQ,)DQ�)DR,)DR�)DS,)DS�)DT,)DT�)DU,)DU�)DV,)DV�)DW,)DW�)DX,)DX�)DY,)DY�)DZ,)DZ�)D[,)D[�)D\,)D\�)D],)D]�)D^,)D^�)D_,)D_�)D`,)D`�)Da,)Da�)Db,)Db�)Dc,)Dc�)Dd,)Dd�)De,)De�)Df,)Df�)Dg,)Dg�)Dh,)Dh�)Di,)Di�)Dj,)Dj�)Dk,)Dk�)Dl,)Dl�)Dm,)Dm�)Dn,)Dn�)Do,)Do�)Dp,)Dp�)Dq,)Dq�)Dr,)Dr�)Ds,)Ds�)Dt,)Dt�)Du,)Du�)Dv,)Dv�)Dw,)Dw�)Dx,)Dx�)Dy,)Dy�)Dz,)Dz�)D{,)D{�)D|,)D|�)D},)D}�)D~,)D~�)D,)D�)D�D�VD��D��D�D�VD��D��D�D�VD��D��D�D�VD��D��D�D�VD��D��D�D�VD��D��D�D�VD��D��D�D�VD��D��D�D�VD��D��D�D�VD��D��D�D�VD��D��D�D�VD��D��D�D�VD��D��D�D�VD��D��D�D�VD��D��D�D�VD��D��D�D�VD��D��D�D�VD��D��D�D�VD��D��D�D�VD��D��D�D�VD��D��D�D�VD��D��D�D�VD��D��D�D�VD��D��D�D�VD��D��D�D�VD��D��D�D�VD��D��D�D�VD��D��D�D�VD��D��D�D�VD��D��D�D�VD��D��D�D�VD��D��D�D�VD��D��D�D�VD��D��D�D�VD��D��D�D�VD��D��D�D�VD��D��D�D�VD��D��D�D�VD��D��D�D�VD��D��D�D�VD��D��D�D�VD��D��D�D�VD��D��D�D�VD��D��D�D�VD��D��D�D�VD��D��D�D�VD��D��D�D�VD��D��D�D�VD��D��D�D�VD��D��D�D�VD��D��D�D�VD��D��D�D�VD��D��D�D�VD��D��D�D�VD��D��D�D�YHD��D��D�D�VD��D��D�D�VD��D��D�D�VD��D��D�D�VD��D��D�D�VD��D��D�D�VD��D��D�D�VD��D��D�D�VD��D��D�D�VD��D��D�D�VD��D��D�D�VDD��D�D�VDÖD��D�D�VDĖD��D�D�VDŖD��D�D�VDƖD��D�D�VDǖD��D�D�VDȖD��D�D�VDɖD��D�D�VDʖD��D�D�VD˖D��D�D�VD̖D��D�D�VD͖D��D�D�YHDΖD��D�D�VDϖD��D�D�VDЖD��D�D�VDіD��D�D�VDҖD��D�D�VDӖD��D�D�VDԖD��D�D�VDՖD��D�D�VD֖D��D�D�R�DזD��D�D�VDؖD��D�D�VDٖD��D�D�VDږD��D�D�VDۖD��D�D�VDܖD��D�D�VDݖD��D�D�VDޖD��D�D�VDߖD��D�D�VD��D��D�D�VD�D��D�D�VD�D��D�D�VD�D��D�D�VD�D��D�D�VD�D��D�D�VD�D��D�D�YHD�HD��D�D�VD�D��D�D�VD�D��D�D�VD�D��D�D�VD�D��D�D�VD�D��D�D�VD�D��D�D�VD�D��D�D�VD�D��D�D�VD�D��D�D�VD�D��D�D�VD�D��D�D�VD�D��D�D�VD��D��D�D�VD��D��D�D�VD��D��D�D�VD��D��D�D�\{D�yH111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   A���AξwAμjAμjAξwA�AζFA���A���A���A���A���A�ƨA�ƨA�ȴA���Aβ-AΥ�AΟ�AΛ�A΍PA·+A�~�A�~�A΍PAμjA��HAΧ�A��A��A�JA���A�  A��A�C�A��`A��9A��A�\)A��A�bNA�VA�x�A�^5A�I�A���A��A�oA�{A��A�(�A�A���A�9XA���A�oA��RA�l�A���A��^A��uA�{A�\)A��9A�A�r�A���A���A�M�A��
A��#A�ĜA�ZA���A��A�M�A���A���A�9XA�~�A��jA��A�=qA���A�C�A�-A�hsA��wA��A��A��HA��#A��9A�K�A�{A��!A�ĜA���A�I�A�$�A���A��A���A�-A�v�A��9A���A��A��A��A~z�A}33Az�+Aw7LAsC�Ap�An�\AlffAi��Ag�hAe�Ac
=Aa"�A_?}A]x�AZQ�AY��AW��AU+AT��AS\)AQ�-AO��ANz�AM7LAK;dAIt�AH�AF�AE�FAD��AA�^A@�9A@I�A>��A;/A9p�A8bNA5�A3�A1�A0A�A.�DA-C�A*�A*��A)p�A'dZA&Q�A%��A%/A$��A$�DA#+A!�hA ��A��A�yAbA��AZA�TA�A�+A�;A`BA��AZA�7AjA��A`BAr�A��A��AA�A1A�hA�A�\A-A�-A��AjAI�A�7A��AK�A
�A
VA
(�A	A�HA5?A�PAt�A�A�+A��A+A��A��A%A��A��A �/@���@���@���@�~�@�V@�Ĝ@��@@��@��-@웦@�\)@�x�@�u@�@��m@�
=@旍@�@�
=@�-@�bN@ߍP@�-@���@�&�@�ƨ@�^5@�p�@�S�@֧�@պ^@�x�@�Q�@��
@��@�G�@Ϯ@Ώ\@��#@�X@��/@��
@�M�@�@ɺ^@���@�x�@��/@ǝ�@��y@�J@�G�@�bN@���@Õ�@��@�~�@�-@���@��9@��m@�l�@���@���@���@�&�@���@��D@��w@���@�M�@�@�?}@��/@���@�hs@��@�bN@�|�@�K�@��!@��@��@�Ĝ@�r�@�1'@��F@�@�ff@��#@�x�@��/@���@���@��@�n�@��H@���@��F@�;d@�+@���@�v�@�E�@�@�%@�%@�%@��/@���@�r�@�r�@���@��@�j@�b@��;@� �@�b@��@�|�@�S�@�ff@�Z@��F@�o@�V@���@��#@���@�@��@�ff@�~�@�n�@��@��7@��@��@�V@��j@�j@�I�@�1@��m@��P@�K�@���@��-@�7L@�?}@�%@��j@���@���@�Z@���@��@�o@��@��+@�@���@�p�@�G�@���@���@�1@�S�@���@���@�@��+@��@�?}@��j@��u@�r�@�9X@���@�dZ@�S�@�+@�@��y@��@���@��\@�^5@�M�@�5?@��@���@�x�@�X@�&�@��`@��9@�A�@���@�ƨ@���@�dZ@�o@��@�v�@�V@�J@���@�?}@�&�@��`@���@�C�@�33@�E�@�V@��D@��j@�bN@�bN@�j@�9X@��w@�
=@�n�@��@��@��@�bN@�Z@�A�@��@���@��@��@�^5@�$�@�@��@�@��@�?}@�&�@��@�V@��@�Ĝ@��@���@��@��
@���@�\)@�K�@��@��H@�ȴ@��R@�n�@�$�@��@���@��7@�p�@�hs@�O�@�&�@�V@���@���@��D@�  @��F@�K�@��@��+@�^5@�=q@�$�@��@���@��-@��h@�p�@�?}@��`@�r�@� �@�1@�;@\)@
=@~��@}@}/@}V@}V@|�@|�D@|9X@{�
@{��@{dZ@{"�@z�@z�@z�H@zn�@z=q@z�@zJ@y��@y�#@x��@xA�@w�@w�@w+@v�+@v5?@v{@v@u�T@u@u/@t�@t(�@s�@r��@r^5@rJ@q��@q7L@pr�@p  @o�@ol�@o�@n�@n�+@n$�@mp�@m?}@l�j@l�@k��@kt�@k33@j��@jM�@i��@i��@i7L@hr�@h�@hbN@g�@gl�@f�y@f��@fV@e�-@eV@dz�@d9X@d1@cƨ@ct�@c"�@b��@b�!@b~�@bM�@a�^@aG�@`�9@`bN@`b@_�w@_�@_\)@^�R@^5?@^$�@]�-@]`B@\�@\j@\9X@\�@[�m@[�m@[��@[��@[�m@[�F@[o@Z�@Z~�@Y��@Y�^@Yx�@YG�@Y&�@X��@X��@X1'@W�@W\)@W;d@W�@V��@Vȴ@V��@Vv�@V$�@V{@U�@U@U`B@T�/@T1@S�
@S��@S��@S�@St�@SS�@S33@S"�@R�!@R�@Q��@Q�7@QG�@Q&�@P�u@P1'@Pb@Pb@O�@O\)@N�y@N��@M�T@M�h@M`B@M?}@L��@L�D@L1@K�@KC�@K33@Ko@J��@J�!@J�\@JM�@JJ@I��@I�#@Ix�@H��@H�9@H�@HA�@G�@G�@Gl�@G+@G
=@FV@E��@E�h@EV@D�@DZ@D1@C�
@CdZ@B��@B��@B~�@Bn�@BM�@B�@A�#@A��@Ax�@A&�@@�9@@Q�@@ �@?�w@?�P@?\)@?
=@>��@>�+@>$�@=�-@=�h@=`B@=?}@=V@<�D@<I�@<(�@;�m@;��@;dZ@:�@:n�@:J@9��@9x�@9hs@9X@97L@8Ĝ@8r�@8Q�@7�@7�@7l�@7�@6��@6��@6��@6�y@6��@65?@5�T@5�-@5��@5p�@5/@5V@4�/@4��@4z�@4j@4Z@4(�@3�
@3��@3dZ@3dZ@3dZ@3S�@3o@2�@2��@2�@1��@1hs@17L@1�@0Ĝ@0r�@0A�@01'@/�;@/|�@/�@.ȴ@.�+@.ff@.ff@.V@.E�@.5?@.{@.@-��@-�h@-�@-p�@-p�@-O�@-�@-�@-V@,�@,�/@,�/@,��@,I�@,1@+��@+S�@+33@*��@*~�@*^5@*=q@*-@*�@)��@)��@)�7@)x�@)X@)&�@(�`@(r�@(A�@(b@(  @'��@'�@'l�@'\)@'
=@&�@&�R@&�R@&�R@&��@&�+@&5?@&$�@%�@%��@%�h@%p�@%`B@%?}@%�@$��@$��@$�D@$I�@#��@#�m@#�
@#�F@#�@#dZ@#33@#@"��@"~�@"^5@"�@!x�@!G�@!%@ Ĝ@ ��@ Q�@   @�w@l�@�@�@��@v�@E�@5?@�T@@��@�@?}@�@V@��@�@z�@Z@Z@Z@j@j@(�@�
@t�@�@�@t�@33@��@��@=q@J@��@�^@��@x�@7L@&�@��@��@��@��@�9@��@�u@r�@A�@A�@1'@ �@ �@�@��@�@l�@\)@K�@;d@�@��@�R@v�@V@$�@�@@@�-@�h@�@`B@V@�j@��@�@��@z�@Z@I�@I�@(�@��@ƨ@ƨ@��@t�@dZ@t�@33@"�@�@�H@��@�!@�\@�\@~�@=q@J@�#@��@�7@hs@�@%@�`@��@Ĝ@�@r�@ �@b@�;@��@�@|�@\)@K�@;d@;d111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   A���AξwAμjAμjAξwA�AζFA���A���A���A���A���A�ƨA�ƨA�ȴA���Aβ-AΥ�AΟ�AΛ�A΍PA·+A�~�A�~�A΍PAμjA��HAΧ�A��A��A�JA���A�  A��A�C�A��`A��9A��A�\)A��A�bNA�VA�x�A�^5A�I�A���A��A�oA�{A��A�(�A�A���A�9XA���A�oA��RA�l�A���A��^A��uA�{A�\)A��9A�A�r�A���A���A�M�A��
A��#A�ĜA�ZA���A��A�M�A���A���A�9XA�~�A��jA��A�=qA���A�C�A�-A�hsA��wA��A��A��HA��#A��9A�K�A�{A��!A�ĜA���A�I�A�$�A���A��A���A�-A�v�A��9A���A��A��A��A~z�A}33Az�+Aw7LAsC�Ap�An�\AlffAi��Ag�hAe�Ac
=Aa"�A_?}A]x�AZQ�AY��AW��AU+AT��AS\)AQ�-AO��ANz�AM7LAK;dAIt�AH�AF�AE�FAD��AA�^A@�9A@I�A>��A;/A9p�A8bNA5�A3�A1�A0A�A.�DA-C�A*�A*��A)p�A'dZA&Q�A%��A%/A$��A$�DA#+A!�hA ��A��A�yAbA��AZA�TA�A�+A�;A`BA��AZA�7AjA��A`BAr�A��A��AA�A1A�hA�A�\A-A�-A��AjAI�A�7A��AK�A
�A
VA
(�A	A�HA5?A�PAt�A�A�+A��A+A��A��A%A��A��A �/@���@���@���@�~�@�V@�Ĝ@��@@��@��-@웦@�\)@�x�@�u@�@��m@�
=@旍@�@�
=@�-@�bN@ߍP@�-@���@�&�@�ƨ@�^5@�p�@�S�@֧�@պ^@�x�@�Q�@��
@��@�G�@Ϯ@Ώ\@��#@�X@��/@��
@�M�@�@ɺ^@���@�x�@��/@ǝ�@��y@�J@�G�@�bN@���@Õ�@��@�~�@�-@���@��9@��m@�l�@���@���@���@�&�@���@��D@��w@���@�M�@�@�?}@��/@���@�hs@��@�bN@�|�@�K�@��!@��@��@�Ĝ@�r�@�1'@��F@�@�ff@��#@�x�@��/@���@���@��@�n�@��H@���@��F@�;d@�+@���@�v�@�E�@�@�%@�%@�%@��/@���@�r�@�r�@���@��@�j@�b@��;@� �@�b@��@�|�@�S�@�ff@�Z@��F@�o@�V@���@��#@���@�@��@�ff@�~�@�n�@��@��7@��@��@�V@��j@�j@�I�@�1@��m@��P@�K�@���@��-@�7L@�?}@�%@��j@���@���@�Z@���@��@�o@��@��+@�@���@�p�@�G�@���@���@�1@�S�@���@���@�@��+@��@�?}@��j@��u@�r�@�9X@���@�dZ@�S�@�+@�@��y@��@���@��\@�^5@�M�@�5?@��@���@�x�@�X@�&�@��`@��9@�A�@���@�ƨ@���@�dZ@�o@��@�v�@�V@�J@���@�?}@�&�@��`@���@�C�@�33@�E�@�V@��D@��j@�bN@�bN@�j@�9X@��w@�
=@�n�@��@��@��@�bN@�Z@�A�@��@���@��@��@�^5@�$�@�@��@�@��@�?}@�&�@��@�V@��@�Ĝ@��@���@��@��
@���@�\)@�K�@��@��H@�ȴ@��R@�n�@�$�@��@���@��7@�p�@�hs@�O�@�&�@�V@���@���@��D@�  @��F@�K�@��@��+@�^5@�=q@�$�@��@���@��-@��h@�p�@�?}@��`@�r�@� �@�1@�;@\)@
=@~��@}@}/@}V@}V@|�@|�D@|9X@{�
@{��@{dZ@{"�@z�@z�@z�H@zn�@z=q@z�@zJ@y��@y�#@x��@xA�@w�@w�@w+@v�+@v5?@v{@v@u�T@u@u/@t�@t(�@s�@r��@r^5@rJ@q��@q7L@pr�@p  @o�@ol�@o�@n�@n�+@n$�@mp�@m?}@l�j@l�@k��@kt�@k33@j��@jM�@i��@i��@i7L@hr�@h�@hbN@g�@gl�@f�y@f��@fV@e�-@eV@dz�@d9X@d1@cƨ@ct�@c"�@b��@b�!@b~�@bM�@a�^@aG�@`�9@`bN@`b@_�w@_�@_\)@^�R@^5?@^$�@]�-@]`B@\�@\j@\9X@\�@[�m@[�m@[��@[��@[�m@[�F@[o@Z�@Z~�@Y��@Y�^@Yx�@YG�@Y&�@X��@X��@X1'@W�@W\)@W;d@W�@V��@Vȴ@V��@Vv�@V$�@V{@U�@U@U`B@T�/@T1@S�
@S��@S��@S�@St�@SS�@S33@S"�@R�!@R�@Q��@Q�7@QG�@Q&�@P�u@P1'@Pb@Pb@O�@O\)@N�y@N��@M�T@M�h@M`B@M?}@L��@L�D@L1@K�@KC�@K33@Ko@J��@J�!@J�\@JM�@JJ@I��@I�#@Ix�@H��@H�9@H�@HA�@G�@G�@Gl�@G+@G
=@FV@E��@E�h@EV@D�@DZ@D1@C�
@CdZ@B��@B��@B~�@Bn�@BM�@B�@A�#@A��@Ax�@A&�@@�9@@Q�@@ �@?�w@?�P@?\)@?
=@>��@>�+@>$�@=�-@=�h@=`B@=?}@=V@<�D@<I�@<(�@;�m@;��@;dZ@:�@:n�@:J@9��@9x�@9hs@9X@97L@8Ĝ@8r�@8Q�@7�@7�@7l�@7�@6��@6��@6��@6�y@6��@65?@5�T@5�-@5��@5p�@5/@5V@4�/@4��@4z�@4j@4Z@4(�@3�
@3��@3dZ@3dZ@3dZ@3S�@3o@2�@2��@2�@1��@1hs@17L@1�@0Ĝ@0r�@0A�@01'@/�;@/|�@/�@.ȴ@.�+@.ff@.ff@.V@.E�@.5?@.{@.@-��@-�h@-�@-p�@-p�@-O�@-�@-�@-V@,�@,�/@,�/@,��@,I�@,1@+��@+S�@+33@*��@*~�@*^5@*=q@*-@*�@)��@)��@)�7@)x�@)X@)&�@(�`@(r�@(A�@(b@(  @'��@'�@'l�@'\)@'
=@&�@&�R@&�R@&�R@&��@&�+@&5?@&$�@%�@%��@%�h@%p�@%`B@%?}@%�@$��@$��@$�D@$I�@#��@#�m@#�
@#�F@#�@#dZ@#33@#@"��@"~�@"^5@"�@!x�@!G�@!%@ Ĝ@ ��@ Q�@   @�w@l�@�@�@��@v�@E�@5?@�T@@��@�@?}@�@V@��@�@z�@Z@Z@Z@j@j@(�@�
@t�@�@�@t�@33@��@��@=q@J@��@�^@��@x�@7L@&�@��@��@��@��@�9@��@�u@r�@A�@A�@1'@ �@ �@�@��@�@l�@\)@K�@;d@�@��@�R@v�@V@$�@�@@@�-@�h@�@`B@V@�j@��@�@��@z�@Z@I�@I�@(�@��@ƨ@ƨ@��@t�@dZ@t�@33@"�@�@�H@��@�!@�\@�\@~�@=q@J@�#@��@�7@hs@�@%@�`@��@Ĝ@�@r�@ �@b@�;@��@�@|�@\)@K�@;d@;d111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
�#B
�BB
�NB
�ZB
�B
�B
��BJB,BG�BffB}�B�DB�{B�-B�}B�dB�B�sB��BĜB�fBB��B��B	7B�B"�B8RBG�BE�BF�BL�BVBYBZB`BBe`BdZBjBhsBgmBhsBhsBffBdZBe`BdZBe`BbNB`BBXBR�BN�BI�BG�B=qB8RB49B%�B�BuBPBB��B�B�ZB�BǮB�?B��B��B�bB�%B�Bt�B`BBXBR�BK�B>wB)�B�B1B
��B
�NB
�)B
�B
��B
�}B
�B
��B
t�B
VB
?}B
)�B
bB	�B	ɺB	�!B	��B	�%B	n�B	`BB	aHB	J�B	A�B	7LB	9XB	�B	�B	JB��B��B�B�HB�ZB�TB�B�BB��B��B�B�B��BǮB�}B�jB�-B�\B�%B�DB�PB�7B�1B�B}�Bz�Bs�Br�Bq�BhsBiyBdZBaHB_;B_;BaHB_;B]/B]/B_;BdZB`BB\)B[#BXBVBVBT�BXB\)BZBW
BS�BP�BP�BQ�BT�BT�BYB^5B`BBdZBgmBhsBhsBm�Bo�Bm�Bl�BjBiyBjBjBhsBdZBaHB]/B_;BbNBcTBn�Bz�B|�B|�B}�Bx�Br�BgmBYBQ�BQ�BM�BA�B>wB>wB@�B?}B?}B>wB<jB8RB5?B6FB7LB7LB8RB9XB6FB6FB5?B8RB:^B9XB8RB6FB7LB7LB7LB8RB8RB9XB=qB;dB;dB>wB=qB>wB>wB?}B@�BC�BF�BI�BK�BM�BP�BQ�BS�BW
BW
BYB\)B^5B`BBe`BgmBiyBl�Bs�Bs�Bt�Bv�Bw�Bz�B}�B~�B�B�B�%B�1B�7B�DB�PB�\B��B��B��B��B��B��B��B��B��B��B�B�B�B�!B�-B�LB�qBȴB�B�;B�5B�TB�B�B�B�B��B��B��B	B	1B	DB	PB	\B	bB	hB	uB	�B	�B	$�B	'�B	+B	/B	49B	8RB	7LB	9XB	9XB	5?B	5?B	7LB	;dB	>wB	@�B	@�B	B�B	F�B	Q�B	R�B	YB	\)B	[#B	\)B	^5B	cTB	ffB	ffB	iyB	l�B	m�B	n�B	p�B	s�B	r�B	u�B	z�B	}�B	�B	�B	�B	�%B	�1B	�DB	�DB	�PB	�hB	�uB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�B	�B	�B	�!B	�'B	�-B	�-B	�-B	�3B	�?B	�?B	�?B	�FB	�LB	�RB	�^B	�dB	�jB	�qB	�wB	�}B	��B	ÖB	ÖB	ĜB	ƨB	ƨB	ǮB	ƨB	ŢB	ÖB	B	ĜB	��B	��B	ĜB	ŢB	ƨB	ȴB	ȴB	ɺB	ǮB	ŢB	B	��B	��B	ĜB	ŢB	ƨB	ǮB	ǮB	ȴB	ǮB	ƨB	ǮB	ǮB	ǮB	ȴB	ɺB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�
B	�B	�#B	�)B	�/B	�/B	�/B	�5B	�;B	�;B	�;B	�BB	�HB	�TB	�ZB	�fB	�mB	�sB	�yB	�yB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
  B
  B
  B
  B
  B
B
B
B
B
B
B
B
B
%B
+B
1B
1B
	7B
	7B

=B

=B

=B
DB
JB
JB
PB
VB
PB
PB
PB
PB
PB
VB
VB
\B
\B
hB
hB
bB
hB
oB
oB
hB
hB
hB
hB
oB
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
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
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
!�B
!�B
!�B
!�B
!�B
!�B
"�B
"�B
#�B
#�B
#�B
#�B
#�B
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
&�B
&�B
&�B
&�B
&�B
'�B
'�B
(�B
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
-B
-B
-B
-B
.B
.B
.B
/B
/B
0!B
0!B
0!B
0!B
0!B
0!B
0!B
0!B
1'B
2-B
2-B
2-B
33B
33B
33B
33B
49B
5?B
5?B
5?B
5?B
6FB
6FB
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
9XB
9XB
9XB
:^B
:^B
:^B
:^B
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
>wB
>wB
>wB
>wB
>wB
?}B
?}B
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
C�B
C�B
C�B
C�B
C�B
C�B
C�B
D�B
D�B
D�B
D�B
D�B
E�B
E�B
E�B
E�B
E�B
E�B
E�B
E�B
F�B
F�B
F�B
G�B
G�B
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
L�B
L�B
L�B
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
P�B
P�B
P�B
P�B
Q�B
Q�B
Q�B
Q�B
R�B
R�B
R�B
R�B
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
T�B
T�B
T�B
T�B
T�B
VB
VB
VB
VB
VB
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
`BB
`BB
`BB
`BB
`BB
`BB
aHB
aHB
bNB
bNB
bNB
bNB
bNB
bNB
bNB
bNB
cTB
cTB
cTB
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
gmB
gmB
gmB
gmB
gmB
hsB
hsB
hsB
iyB
iyB
iyB
iyB
iyB
jB
iyB
iyB
iyB
iyB
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
l�B
m�B
m�B
m�B
m�B
n�B
n�B
n�B
n�B
n�B
n�B
n�B
o�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   B
˒B
˒B
˒B
̘B
̘B
̘B
�~B
ʌB
ʌB
˒B
˒B
˒B
ϫB
ΥB
οB
��B
�	B
�B
�4B
�@B
�eB
�}B
��B�B+�BG�BgBcB�B�$B��B�oB��BیB��B�"BƎB�B	B�B��B)B�B%`B;0BIBGzBH�BNVBXEBZB\�Bb�BgBgRBl�BiDBiBjeBjeBg�Be�Bf�Be�Bf�Bc�Ba�BY1BTFBQBLBJ#B>�B9�B5�B&�BdB{B�B�B�JB�|B��B��B��B��B�yB��B�hB��B�Bv�B`�BX�BS�BM�B@OB+kB!B
�B
��B
��B
�/B
�sB
̈́B
�UB
��B
�xB
w�B
X�B
A�B
-]B
FB	�B	�~B	��B	�=B	�B	q'B	b�B	cnB	MB	C�B	9�B	<6B	 B	�B	�B��B�FB�|B�B�B��B�qB��B�2BյB�BںB՛B��B�iB��B��B��B��B�<B�HB�B��B�B�B}BtnBt9Bs�Bi�Bi�Bd�Ba�B_�B`�Bb�B`'B^5B^B`BBezB`�B\�B[�BXyBV�BVmBU�BX�B]B[#BW�BT�BQ�BQ�BR�BUBU2BYB^�B`�Bd�Bg�Bh�Bh�Bm�BpoBn�Bm�BkBi�Bj�Bj�BiDBd�Ba�B]IB_�BbNBb�BnIB{JB~(B}�BHBz^Bu%Bi�BZQBR�BS�BO�BBAB>�B>�B@�B@ B@ B?.B="B8�B5?B6zB7�B7�B9rB:B6�B6�B5�B8�B:xB9�B8�B6�B7�B8RB7�B8�B8lB9�B=�B;�B<jB?.B=�B>�B>�B?�B@�BDBF�BI�BK�BM�BQBR�BT,BWYBW?BYKB\)B^OB`vBezBg�Bi�Bl�Bs�Bs�Bt�Bv�BxBz�B}�BB�[B��B�?B�1B�RB�)B�B��B��B��B��B�sB��B�B��B��B��B��B�B�/B�IB�;B�-B�B��BǮB��B�;BݲB�B�CB�B��B�B��B��B�B	9B	�B	B	B	(B	.B	B	&B	WB	�B	$�B	'�B	*�B	/ B	49B	8B	7fB	9�B	:DB	5tB	5tB	7�B	;0B	>BB	@4B	@4B	B'B	F%B	Q�B	R�B	YB	\B	Z�B	\B	^B	c:B	fLB	fLB	iDB	lqB	mwB	n�B	p�B	tB	r�B	utB	z�B	}�B	��B	��B	�B	�?B	�B	�DB	�)B	�PB	�hB	�uB	�mB	�mB	��B	��B	��B	��B	��B	�QB	�dB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	��B	�B	�B	�%B	�+B	�2B	�RB	�DB	�0B	�6B	�VB	�]B	�HB	�iB	�aB	ÖB	āB	ƎB	�tB	ǔB	��B	�B	�{B	��B	��B	�iB	� B	āB	�SB	�tB	ȀB	��B	��B	��B	ŢB	��B	�oB	�;B	�MB	�mB	�tB	ǔB	ǮB	ȴB	��B	ƎB	�zB	�_B	ǔB	ȚB	ɆB	�rB	�rB	˒B	˒B	˒B	�~B	̘B	��B	��B	��B	ѷB	ңB	��B	��B	��B	��B	��B	��B	��B	�	B	��B	��B	��B	��B	�B	�B	�B	�!B	�'B	�bB	�TB	�ZB	�fB	�RB	�$B	�DB	�*B	�KB	�0B	�QB	�QB	�6B	�QB	�qB	�B	�cB	�iB	�iB	�oB	�vB	�B	�B	�B	�nB	�tB	�tB	��B	�zB	��B	��B	��B	��B	��B	�lB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
 �B
 �B
 �B
�B
�B
�B
�B
B
B
B
�B
�B
	B
	B

	B

#B

	B
B
B
0B
B
"B
B
B
B
6B
6B
<B
"B
(B
(B
4B
NB
.B
4B
:B
:B
hB
4B
NB
4B
 B
 B
:B
&B
@B
&B
,B
,B
FB
gB
gB
9B
SB
YB
YB
sB
YB
yB
KB
B
B
B
kB
qB
qB
WB
=B
WB
=B
qB
WB
qB
]B
�B
~B
dB
�B
�B
�B
pB
pB
�B
�B
 �B
 vB
 vB
 �B
 �B
 �B
!|B
!�B
!|B
!�B
!�B
!�B
"�B
"�B
#�B
#�B
#nB
#�B
#�B
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
&�B
&�B
&�B
&�B
&�B
'�B
'�B
(�B
)�B
)�B
)�B
)�B
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
-�B
-�B
/ B
.�B
/�B
/�B
/�B
/�B
/�B
/�B
/�B
0!B
1B
1�B
2B
2B
2�B
2�B
2�B
3B
4B
5B
5B
4�B
5B
5�B
5�B
5�B
6B
5�B
6+B
6�B
7B
72B
8B
8B
8B
9$B
9	B
9$B
9>B
:B
:*B
:*B
:*B
;0B
;B
;B
<6B
<6B
<B
<6B
=VB
=<B
="B
>BB
>(B
>(B
>BB
>BB
?HB
?.B
@4B
@OB
@4B
AUB
A;B
A;B
A B
AUB
A;B
B[B
B[B
CGB
CGB
CaB
CaB
CGB
CGB
CGB
DgB
D3B
DgB
DMB
DMB
ESB
ESB
E9B
ESB
EmB
EmB
EmB
E�B
FtB
FtB
FtB
GzB
GzB
GzB
G_B
HfB
HfB
H�B
H�B
H�B
I�B
IlB
I�B
JrB
I�B
IlB
JrB
JrB
JrB
J�B
J�B
JrB
JXB
JrB
JrB
KxB
K^B
KxB
LdB
L~B
LdB
L�B
L�B
L~B
L�B
L~B
L�B
L�B
M�B
M�B
M�B
N�B
N�B
N�B
N�B
NpB
N�B
N�B
N�B
O�B
P�B
P�B
P�B
P�B
P�B
P�B
Q�B
Q�B
Q�B
Q�B
R�B
R�B
R�B
R�B
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
T�B
T�B
T�B
T�B
T�B
U�B
U�B
U�B
U�B
U�B
U�B
U�B
U�B
U�B
U�B
U�B
V�B
V�B
V�B
V�B
V�B
V�B
V�B
V�B
V�B
W�B
X�B
X�B
X�B
X�B
X�B
Y�B
Y�B
Y�B
Y�B
Y�B
Y�B
Z�B
Z�B
Z�B
Z�B
Z�B
Z�B
[�B
[�B
[�B
[�B
\�B
\�B
]�B
]�B
^B
]�B
^B
]�B
_B
^�B
^�B
_�B
`B
`B
_�B
_�B
`B
`�B
`�B
a�B
a�B
a�B
a�B
bB
a�B
a�B
a�B
cB
b�B
c B
d&B
dB
eB
d�B
eB
d�B
e,B
eB
e,B
e,B
eB
e,B
e,B
f2B
fB
f2B
gB
gB
g8B
gB
g8B
h
B
h$B
h$B
iDB
iDB
i*B
i*B
iDB
jKB
iDB
i*B
iDB
i*B
jB
j0B
j0B
jKB
k6B
kB
kB
kQB
k6B
k6B
k6B
k6B
kQB
kQB
k6B
k6B
kQB
lWB
l"B
lWB
l"B
lWB
lWB
mCB
m]B
mCB
mCB
n/B
ncB
nIB
nIB
nIB
n/B
n/B
o5111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   @��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
PRES            TEMP            PSAL            PRES            TEMP            PSAL            PRES            TEMP            PSAL            PRES_ADJ=PRES-SP,  where SP is SURFACE PRESSURE from next cycle; PRES_ADJ_ERR=Manufacturer sensor accuracy                                                                                                                                                      TEMP_ADJ=TEMP; TEMP_ADJ_ERR=Manufacturer sensor accuracy                                                                                                                                                                                                        PSAL_ADJ = RecalS= psal(PRES_ADJ,TEMP,Conductivity)                                                                                                                                                                                                             None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            PSAL_ADJ = celltm_sbe41(RecalS,TEMP,P,elapsed_time,alpha,tau); elapsed_time=P/mean_rise_rate; P=dbar since the start of the profile for each samples                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            PSAL_ADJ_ERR=max(RecalS & CTM error, 0.01(PSS-78))                                                                                                                                                                                                              SP=-0.69(dbar)                                                                                                                                                                                                                                                  None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            alpha=0.0267C, tau=18.6s, mean_rise_rate = 0.09 dbar/second                                                                                                                                                                                                     None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            Pressure Correction using reported SURFACE PRESSURE                                                                                                                                                                                                             None                                                                                                                                                                                                                                                            Salinity Recalculation using PRES_ADJ                                                                                                                                                                                                                           None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            Correction for conductivity cell thermal mass error(CTM), Johnson et al., 2007, JAOT                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            No adjustment is needed                                                                                                                                                                                                                                         201911290034012019112900340120191129003401202306231719102023062317191020230623171910201911300022482019113000224820191130002248  JA  ARFMdecpA19c                                                                20191124153710  IP                  G�O�G�O�G�O�                JA  ARFMfmtp2.8                                                                 20191124063714  IP                  G�O�G�O�G�O�                JA  ARCAsspa3.1b                                                                20191124063716  IP  PRES            G�O�G�O�G�O�                JA  ARGQrqcppo_c                                                                20191124063717  QCP$                G�O�G�O�G�O�              3CJA  ARGQrqcpt19d                                                                20191124063718  QCP$                G�O�G�O�G�O�           80000JA  ARGQaqcpt19d                                                                20191124063718  QCP$                G�O�G�O�G�O�           80000JA  ARGQrqcp2.8e                                                                20191124063718  QCP$                G�O�G�O�G�O�            FB40JA  ARGQaqcp2.8e                                                                20191124063718  QCP$                G�O�G�O�G�O�            FB40JA  ARGQrqcpt16c                                                                20191124063719  QCP$                G�O�G�O�G�O�           10000JA      jafc1.0                                                                 20191124063720                      G�O�G�O�G�O�                JA  ARUP                                                                        20191124065440                      G�O�G�O�G�O�                JM  ARGQJMQC2.0                                                                 20191124153832  CV  JULD            G�O�G�O�F�r>                JM  ARCAJMQC2.0                                                                 20191128153401  CV  PRES_ADJUSTED   G�O�G�O�G�O�                JM  ARCAJMQC2.0                                                                 20191128153401  CV  PSAL_ADJUSTED   G�O�G�O�G�O�                JM  ARSQOW  1.1 2017V1                                                          20191129152248  IP  PSAL_ADJUSTED   G�O�G�O�G�O�                JM  ARCAJMTM1.0                                                                 20230623081910  CV  PSAL_ADJUSTED   G�O�G�O�G�O�                JA  ARDU                                                                        20230705031506                      G�O�G�O�G�O�                