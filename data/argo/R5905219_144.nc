CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       JMA    source        
Argo float     history       E2022-04-18T14:53:51Z creation;2022-04-18T14:53:53Z conversion to V3.1      
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
_FillValue                 �  I   PRES_ADJUSTED            
      	   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  L�   PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  \�   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     
_FillValue        G�O�   	valid_min         �      	valid_max         B      C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  `h   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  o�   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     
_FillValue        G�O�   	valid_min         �      	valid_max         B      C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  s�   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �l   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    
_FillValue        G�O�   	valid_min         @      	valid_max         B$     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  �P   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ��   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    
_FillValue        G�O�   	valid_min         @      	valid_max         B$     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  ��   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �T   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         decibar    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  �8   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         degree_Celsius     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  ��   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  �X   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  0  ��   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                    �   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                    �   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                    �   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  ,  �   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                    �D   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                    �H   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                    �L   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                    �P   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  @  �T   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                    �   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                    �   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    �   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar         �   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar         �   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�        ��   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    ��Argo profile    3.1 1.2 19500101000000  20220418145351  20220419211501  5905219                                                                 JAMSTEC                                                         PRES            TEMP            PSAL               �A   JA                                  2B  A   APEX                            7906                            051216                          846 @����}'�1   @��`�@4�+J�dO|�hs1   GPS     A   A   A   Primary sampling: averaged [2 dbar bin average for 1 Hz CTD]                                                                                                                                                                                                       @@  @�  @�  A   A   A@  A`  A���A���A�  A�  A�  A�  A�  A�  B   B  B  B  B   B(  B0  B8  B@  BH  BP  BX  B`  Bh  Bp  Bx  B�  B�  B�  B�  B�33B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�33B�33B�  B�  C   C  C  C  C  C
  C  C  C  C  C  C  C  C  C  C  C   C"  C$  C&  C(  C)�fC,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Ca�fCc�fCf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C��C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D��D� DfD� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D�fD  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!fD!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D5��D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<y�D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DB��DCy�DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR�fDS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_�fD`  D`� Da  Da� Db  Dby�Dc  Dc� Dd  Dd� De  De�fDf  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dt� Du  Du� Dv  Dv� Dw  Dw� Dx  Dx� Dy  Dy� Dz  Dz� D{  D{� D|  D|� D}  D}� D~  D~� D  D� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�C3D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D���D�  D�@ D�� D�� D���D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�3D�C3D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D��3D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�3D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D��3D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�C3D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�3D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D���D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D D�� D�  D�@ DÀ D�� D�  D�@ DĀ D�� D�  D�@ Dŀ D�� D�  D�@ Dƀ D�� D�  D�@ Dǀ D�� D�  D�@ DȀ D�� D�  D�@ Dɀ D�� D�  D�@ Dʀ D�� D�  D�@ Dˀ D�� D�  D�@ D̀ D�� D�  D�@ D̀ D��3D�  D�@ D΀ D�� D�  D�@ Dπ D�� D�  D�@ DЀ D�� D�  D�@ Dр D�� D�  D�C3DҀ D�� D�  D�@ DӀ D�� D�  D�@ DԀ D�� D�  D�@ DՀ D�� D�  D�@ Dր D�� D�  D�@ D׃3D��3D�  D�@ D؀ D�� D�3D�@ Dـ D�� D�  D�@ Dڀ D�� D�  D�@ Dۀ D�� D�  D�@ D܀ D�� D�  D�@ D݀ D�� D�  D�@ Dހ D�� D�  D�@ D߀ D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�<�D� D�� D�  D�@ D�3D��3D�  D�@ D� D�� D�  D�@ D�3D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D��3D�  D�<�D� D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�<�D� D��3D�  D�<�D� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D��f111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111@^{@�
=@�
=A�A'�AG�Ag�A��\A��\A�A�A�A�A�A�B�HB	�HB�HB�HB!�HB)�HB1�HB9�HBA�HBI�HBQ�HBY�HBa�HBi�HBq�HBy�HB��B��B��B��B�#�B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B�#�B�#�B��B��C xRCxRCxRCxRCxRC
xRCxRCxRCxRCxRCxRCxRCxRCxRCxRCxRC xRC"xRC$xRC&xRC(xRC*^�C,xRC.xRC0xRC2xRC4xRC6xRC8xRC:xRC<xRC>xRC@xRCBxRCDxRCFxRCHxRCJxRCLxRCNxRCPxRCRxRCTxRCVxRCXxRCZxRC\xRC^xRC`xRCb^�Cd^�CfxRChxRCjxRClxRCnxRCpxRCrxRCtxRCvxRCxxRCzxRC|xRC~xRC�<)C�<)C�<)C�<)C�<)C�H�C�H�C�<)C�<)C�<)C�<)C�<)C�<)C�<)C�<)C�<)C�<)C�<)C�<)C�<)C�<)C�<)C�<)C�<)C�<)C�<)C�<)C�<)C�<)C�<)C�<)C�<)C�<)C�<)C�<)C�<)C�<)C�<)C�<)C�<)C�<)C�<)C�H�C�<)C�<)C�<)C�<)C�<)C�<)C�<)C�<)C�<)C�<)C�<)C�<)C�<)C�<)C�<)C�<)C�<)C�<)C�<)C�<)C�<)C�<)C�<)C�<)C�<)C�<)C�H�C�<)C�<)C�<)C�<)C�<)C�<)C�<)C�<)C�<)C�<)C�<)C�<)C�<)C�<)C�<)C�<)C�<)C�<)C�<)C�<)C�<)C�<)C�<)C�<)C�<)C�<)C�<)C�<)C�<)C�H�C�<)C�<)C�<)C�<)C�<)C�<)C�<)C�<)C�<)C�<)C�<)C�<)C�<)C�<)C�<)C�<)C�<)C�<)C�<)C�<)C�<)C�<)C�<)C�<)C�<)C�<)C�<)C�<)D D �DD�DD�D�D�D$zD�DD�DD�DD�DD�D	D	�D
D
�DD�DD�DD�DD�DD�DD�DD�DD�DD�DD�DD�DD�zDD�DD�DD�DD�DD�DD�DD�DD�DD�D D �D!$zD!�D"D"�D#D#�D$D$�D%D%�D&D&�D'D'�D(D(�D)D)�D*D*�D+D+�D,D,�D-D-�D.D.�D/D/�D0D0�D1D1�D2D2�D3D3�D4D4�D5D5�D6�D6�D7D7�D8D8�D9D9�D:D:�D;D;�D<D<��D=D=�D>D>�D?D?�D@D@�DADA�DBDB�DC�DC��DDDD�DEDE�DFDF�DGDG�DHDH�DIDI�DJDJ�DKDK�DLDL�DMDM�DNDN�DODO�DPDP�DQDQ�DRDR�zDSDS�DTDT�DUDU�DVDV�DWDW�DXDX�DYDY�DZDZ�D[D[�D\D\�D]D]�D^D^�D_D_�zD`D`�DaDa�DbDb��DcDc�DdDd�DeDe�zDfDf�DgDg�DhDh�DiDi�DjDj�DkDk�DlDl�DmDm�DnDn�DoDo�DpDp�DqDq�DrDr�DsDs�DtDt�DuDu�DvDv�DwDw�DxDx�DyDy�DzDz�D{D{�D|D|�D}D}�D~D~�DD�D�
D�O
D��
D��
D�
D�O
D��
D��
D�
D�O
D��
D��
D�
D�O
D��
D��
D�
D�O
D��
D��
D�
D�O
D��
D��
D�
D�O
D��
D��
D�
D�O
D��
D��
D�
D�R=D��
D��
D�
D�O
D��
D��
D�
D�O
D��
D��
D�
D�O
D��
D��
D�
D�O
D��
D��
D�
D�O
D��
D��
D�
D�O
D��
D��
D�
D�O
D��
D��
D�
D�O
D��
D��
D�
D�O
D��
D���D�
D�O
D��
D��
D��D�O
D��
D��
D�
D�O
D��
D��
D�
D�O
D��
D��
D�
D�O
D��
D��
D�
D�O
D��
D��
D�
D�O
D��
D��
D�=D�R=D��
D��
D�
D�O
D��
D��
D�
D�O
D��
D��
D�
D�O
D��
D��
D�
D�O
D��
D��
D�
D�O
D��
D��
D�
D�O
D��
D��
D�
D�O
D��
D��
D�
D�O
D��
D��
D�
D�O
D��
D��
D�
D�O
D��
D��
D�
D�O
D��
D��
D�
D�O
D��=D��
D�
D�O
D��
D��
D�
D�O
D��
D��
D�
D�O
D��
D��
D�
D�O
D��
D��
D�=D�O
D��
D��
D�
D�O
D��
D��
D�
D�O
D��
D��
D�
D�O
D��
D��
D�
D�O
D��
D��
D�
D�O
D��
D��
D�
D�O
D��
D��
D�
D�O
D��
D��
D�
D�O
D��
D��
D�
D�O
D��
D��
D�
D�O
D��
D��
D�
D�O
D��
D��=D�
D�O
D��
D��
D�
D�O
D��
D��
D�
D�R=D��
D��
D�
D�O
D��
D��
D�
D�O
D��
D��
D�=D�O
D��
D��
D�
D�O
D��
D��
D�
D�O
D��
D���D�
D�O
D��
D��
D�
D�O
D��
D��
D�
D�O
D��
D��
D�
D�O
D��
D��
D�
D�O
D
D��
D�
D�O
DÏ
D��
D�
D�O
Dď
D��
D�
D�O
Dŏ
D��
D�
D�O
DƏ
D��
D�
D�O
DǏ
D��
D�
D�O
Dȏ
D��
D�
D�O
Dɏ
D��
D�
D�O
Dʏ
D��
D�
D�O
Dˏ
D��
D�
D�O
D̏
D��
D�
D�O
D͏
D��=D�
D�O
DΏ
D��
D�
D�O
DϏ
D��
D�
D�O
DЏ
D��
D�
D�O
Dя
D��
D�
D�R=Dҏ
D��
D�
D�O
Dӏ
D��
D�
D�O
Dԏ
D��
D�
D�O
DՏ
D��
D�
D�O
D֏
D��
D�
D�O
Dג=D��=D�
D�O
D؏
D��
D�=D�O
Dُ
D��
D�
D�O
Dڏ
D��
D�
D�O
Dۏ
D��
D�
D�O
D܏
D��
D�
D�O
Dݏ
D��
D�
D�O
Dޏ
D��
D�
D�O
Dߏ
D��
D�
D�O
D��
D��
D�
D�O
D�
D��
D�
D�O
D�
D��
D�
D�O
D�
D��
D�
D�O
D�
D��
D�
D�K�D�
D��
D�
D�O
D�=D��=D�
D�O
D�
D��
D�
D�O
D�=D��
D�
D�O
D�
D��
D�
D�O
D�
D��
D�
D�O
D�
D��
D�
D�O
D�
D��
D�
D�O
D�
D��
D�
D�O
D�
D��=D�
D�K�D�
D��
D�
D�O
D��
D��
D�
D�O
D�
D��
D�
D�O
D�
D��
D�
D�K�D�
D��=D�
D�K�D�
D��
D�
D�O
D��
D��
D�
D�O
D��
D��
D�
D�O
D��
D��
D�
D�O
D��
D��
D�p111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111A�z�A�dZA�K�A�ZA�(�A�{A�%A���A���A��A��A���A��A��A��A��A��;AΣ�A�XA��A��A�XA�1'A���A�ȴAʩ�Aʏ\A�hsA�33A��A�ĜAɺ^Aɉ7A�O�Aȇ+A�=qAǙ�A��A�bA��A�AƸRA�M�A�1A��AŅA�K�A�%Aġ�A���A�=qADA�A�ZA�  A��A�oA��PA��
A��^A�  A���A�K�A��A�M�A��jA�I�A���A�dZA�A�A��!A���A��A��HA���A�  A�p�A���A�%A��A���A�ĜA�jA�v�A�n�A�K�A�K�A�`BA�5?A�7LA��A�"�A��-A��^A�hsA�JA���A�S�A���A�x�A�1A~�\A}7LA{dZAz�9Ay�7Aw"�Atn�Aq�#Ao��Al��Aj�!Ah�uAfM�AcAb��A`$�A^��A[\)AXI�AW��AU��ASl�AR��ARz�AR{AN�AKAJ9XAI
=AGdZAE��AE�hAE7LAD�RAC|�AA�PA?�wA<��A;?}A:E�A9��A9&�A8ȴA8Q�A6�`A6VA5�
A4�/A3ƨA1�^A/�A.ȴA.~�A. �A-��A+l�A*5?A(��A'dZA'�A&��A&A�A%�A$�A#dZA!ƨA 5?A��A  A��At�A��A��A�A�AXA~�Ap�A��A�AdZAQ�A�;A��A�TA�/A$�AhsAjA~�AƨA��A1'A��AhsA
n�A9XAr�A��A�A��A5?Ap�A/A��AQ�A��A"�A ��A �!A 5?A b@�|�@�^5@�z�@��@��@�ȴ@���@�~�@���@�@��-@�ƨ@�v�@���@��@��@��@�%@��@�~�@旍@���@��T@�X@�V@�l�@�ƨ@ڧ�@�-@���@�"�@ְ!@�^5@��@Ԭ@�|�@��@җ�@щ7@�1'@�o@�E�@��`@�(�@���@��;@˕�@�J@�x�@��@�r�@Ǿw@�@�V@ŉ7@��`@�  @Å@�K�@���@�n�@�V@�ȴ@�^5@�@�hs@�j@�1@�|�@�\)@�S�@�S�@�K�@�33@�@��+@�p�@��/@���@�j@�1'@��F@�dZ@�\)@�"�@��+@���@��h@�`B@��@��D@���@�;d@�ȴ@��\@�M�@��@�&�@��@�j@� �@��;@���@��@��@��y@���@���@���@��w@��F@��F@��F@�~�@�?}@���@�Ĝ@��@�/@���@��j@�  @��@�\)@��@���@�|�@�l�@�dZ@��+@��@��@��-@�p�@�&�@�V@�&�@�7L@�7L@��9@���@��j@���@��@� �@�\)@�E�@���@�?}@��D@� �@��m@�1'@��@���@��@�G�@�(�@� �@��@�M�@�I�@��;@�l�@�K�@��@�v�@��@�@��-@�?}@���@�Ĝ@���@��u@��@�j@�Z@�A�@�(�@��F@�S�@�"�@��@�v�@�$�@�{@�J@���@��^@�@���@��h@�X@�/@�%@��9@�Z@�1'@���@���@�S�@�@���@���@��\@��+@�n�@�M�@�5?@��@���@�7L@���@�r�@�(�@�1'@��@�1@���@���@�;d@��H@���@��\@��+@�~�@�v�@�M�@��@�x�@��@���@��j@�r�@���@�b@�1@�\)@���@��R@��\@�n�@�-@���@���@���@���@�x�@�7L@��@��@�z�@�b@�  @��@�ƨ@�t�@�dZ@�dZ@�;d@�o@�
=@���@���@��R@���@�v�@�@�@��@�hs@�x�@�&�@���@�V@�Ĝ@���@�z�@�(�@��F@�l�@�\)@�"�@��H@�v�@�^5@�@�@�x�@�hs@�7L@���@���@���@��u@�j@�  @�|�@�;d@�
=@��!@�v�@��@��T@�@���@��h@�x�@�O�@�7L@�Ĝ@�z�@�I�@�1'@� �@�P@~�y@~v�@~$�@}@}V@|j@{�F@{�@{dZ@{S�@z��@z=q@zJ@y�@x�`@xr�@xb@w��@w�w@wl�@w+@v�y@vv�@v{@u�h@up�@uO�@uV@t�@t�D@t9X@t�@t1@sƨ@sS�@s"�@r�@q�7@qX@q&�@p��@p�u@o�;@o�@n$�@m��@m/@l9X@k�m@k�m@kdZ@j��@j~�@jM�@jM�@j�@i�7@i�@h�`@hĜ@h�u@h�@hbN@hQ�@hA�@hb@g�P@gK�@fȴ@fV@e�@eO�@d��@d��@d�@dj@dZ@dI�@d9X@c�
@c��@c�@c33@b�@b��@bM�@a��@a7L@`�u@`1'@_��@_
=@^ȴ@^ff@^$�@]��@]�-@]�@]?}@\�j@\j@\(�@\�@\1@[�m@[t�@Z�H@Z�!@Z~�@ZJ@Y��@Y�@Y�#@Y��@Y��@Yx�@Yx�@YG�@X��@X�9@X��@X�@XQ�@X �@W�;@W��@W��@Wl�@W\)@W+@W
=@Vȴ@V5?@U��@U`B@U�@T�@T�@TZ@T(�@T1@S�
@Sƨ@SdZ@SC�@R��@RM�@R�@Q��@Q�#@Q��@Qx�@Q7L@Q%@PbN@O�;@O�@Ol�@O;d@N�y@Nȴ@N��@NE�@N{@M�T@M@M��@M��@M�h@Mp�@MV@L�@L�D@Lj@LI�@K��@K�F@K��@K�@KdZ@K@J��@J=q@JJ@Ihs@I%@H��@H�u@H  @G�@G|�@G;d@G�@Fȴ@Fv�@F5?@E�h@E`B@E?}@Dj@C�m@Cƨ@C��@C"�@Co@B�H@B��@BM�@A�7@Ax�@Ahs@A&�@@��@?�;@?��@?K�@>��@>�R@>ff@>{@=�@=�-@=/@=V@<�/@<Z@;�m@;��@;33@:��@:=q@:-@9��@9�7@9�@8Ĝ@8b@7��@7+@6��@6��@6ff@5��@5`B@4�/@4��@4�@3ƨ@3��@3�@3@2��@2~�@2n�@2=q@1��@1�#@1��@1hs@0��@0�9@0r�@0  @/�w@/�P@/;d@/+@/
=@.��@.E�@-�@-�-@-?}@,��@,�@,I�@+��@+�m@+ƨ@+��@+��@+�@+t�@+C�@*�H@*��@*�\@*-@)�#@)��@)��@)��@)hs@)7L@)�@(�9@(r�@(Q�@(A�@(  @'��@'��@'K�@'�@&�R@&ff@&5?@%��@%��@%�h@%�@%p�@%`B@%`B@%/@$��@$�D@$j@$1@#ƨ@#��@#t�@#dZ@#dZ@#S�@#C�@#"�@"��@"��@"�\@"n�@"�@!��@!�@!x�@!&�@ ��@ Ĝ@ �@ Q�@ A�@ 1'@ b@ b@��@��@|�@\)@K�@K�@+@�y@ȴ@�R@��@5?@�T@@��@O�@��@�/@��@�j@�j@�@��@�D@z�@j@I�@(�@��@ƨ@��@t�@"�@�H@��@��@^5@M�@=q@=q@�@�^@�7@hs@G�@7L@7L@7L@%@�`@bN@  @�w@�P@|�@l�@l�@\)@;d@;d@;d@
=@�y@ȴ@�R@��@E�@@�T@��@@�-@p�@/@V@��@�@�D@z�@Z@�@1@��@��@33@33@"�@�@��@��@�\@n�@^5@-@��@��@hs@7L@&�@�@��@��@�9@�@Q�@1'@  @�;@�@�@�P@l�@K�@;d@�@�@ȴ@��@v�@V@{@111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111A�z�A�dZA�K�A�ZA�(�A�{A�%A���A���A��A��A���A��A��A��A��A��;AΣ�A�XA��A��A�XA�1'A���A�ȴAʩ�Aʏ\A�hsA�33A��A�ĜAɺ^Aɉ7A�O�Aȇ+A�=qAǙ�A��A�bA��A�AƸRA�M�A�1A��AŅA�K�A�%Aġ�A���A�=qADA�A�ZA�  A��A�oA��PA��
A��^A�  A���A�K�A��A�M�A��jA�I�A���A�dZA�A�A��!A���A��A��HA���A�  A�p�A���A�%A��A���A�ĜA�jA�v�A�n�A�K�A�K�A�`BA�5?A�7LA��A�"�A��-A��^A�hsA�JA���A�S�A���A�x�A�1A~�\A}7LA{dZAz�9Ay�7Aw"�Atn�Aq�#Ao��Al��Aj�!Ah�uAfM�AcAb��A`$�A^��A[\)AXI�AW��AU��ASl�AR��ARz�AR{AN�AKAJ9XAI
=AGdZAE��AE�hAE7LAD�RAC|�AA�PA?�wA<��A;?}A:E�A9��A9&�A8ȴA8Q�A6�`A6VA5�
A4�/A3ƨA1�^A/�A.ȴA.~�A. �A-��A+l�A*5?A(��A'dZA'�A&��A&A�A%�A$�A#dZA!ƨA 5?A��A  A��At�A��A��A�A�AXA~�Ap�A��A�AdZAQ�A�;A��A�TA�/A$�AhsAjA~�AƨA��A1'A��AhsA
n�A9XAr�A��A�A��A5?Ap�A/A��AQ�A��A"�A ��A �!A 5?A b@�|�@�^5@�z�@��@��@�ȴ@���@�~�@���@�@��-@�ƨ@�v�@���@��@��@��@�%@��@�~�@旍@���@��T@�X@�V@�l�@�ƨ@ڧ�@�-@���@�"�@ְ!@�^5@��@Ԭ@�|�@��@җ�@щ7@�1'@�o@�E�@��`@�(�@���@��;@˕�@�J@�x�@��@�r�@Ǿw@�@�V@ŉ7@��`@�  @Å@�K�@���@�n�@�V@�ȴ@�^5@�@�hs@�j@�1@�|�@�\)@�S�@�S�@�K�@�33@�@��+@�p�@��/@���@�j@�1'@��F@�dZ@�\)@�"�@��+@���@��h@�`B@��@��D@���@�;d@�ȴ@��\@�M�@��@�&�@��@�j@� �@��;@���@��@��@��y@���@���@���@��w@��F@��F@��F@�~�@�?}@���@�Ĝ@��@�/@���@��j@�  @��@�\)@��@���@�|�@�l�@�dZ@��+@��@��@��-@�p�@�&�@�V@�&�@�7L@�7L@��9@���@��j@���@��@� �@�\)@�E�@���@�?}@��D@� �@��m@�1'@��@���@��@�G�@�(�@� �@��@�M�@�I�@��;@�l�@�K�@��@�v�@��@�@��-@�?}@���@�Ĝ@���@��u@��@�j@�Z@�A�@�(�@��F@�S�@�"�@��@�v�@�$�@�{@�J@���@��^@�@���@��h@�X@�/@�%@��9@�Z@�1'@���@���@�S�@�@���@���@��\@��+@�n�@�M�@�5?@��@���@�7L@���@�r�@�(�@�1'@��@�1@���@���@�;d@��H@���@��\@��+@�~�@�v�@�M�@��@�x�@��@���@��j@�r�@���@�b@�1@�\)@���@��R@��\@�n�@�-@���@���@���@���@�x�@�7L@��@��@�z�@�b@�  @��@�ƨ@�t�@�dZ@�dZ@�;d@�o@�
=@���@���@��R@���@�v�@�@�@��@�hs@�x�@�&�@���@�V@�Ĝ@���@�z�@�(�@��F@�l�@�\)@�"�@��H@�v�@�^5@�@�@�x�@�hs@�7L@���@���@���@��u@�j@�  @�|�@�;d@�
=@��!@�v�@��@��T@�@���@��h@�x�@�O�@�7L@�Ĝ@�z�@�I�@�1'@� �@�P@~�y@~v�@~$�@}@}V@|j@{�F@{�@{dZ@{S�@z��@z=q@zJ@y�@x�`@xr�@xb@w��@w�w@wl�@w+@v�y@vv�@v{@u�h@up�@uO�@uV@t�@t�D@t9X@t�@t1@sƨ@sS�@s"�@r�@q�7@qX@q&�@p��@p�u@o�;@o�@n$�@m��@m/@l9X@k�m@k�m@kdZ@j��@j~�@jM�@jM�@j�@i�7@i�@h�`@hĜ@h�u@h�@hbN@hQ�@hA�@hb@g�P@gK�@fȴ@fV@e�@eO�@d��@d��@d�@dj@dZ@dI�@d9X@c�
@c��@c�@c33@b�@b��@bM�@a��@a7L@`�u@`1'@_��@_
=@^ȴ@^ff@^$�@]��@]�-@]�@]?}@\�j@\j@\(�@\�@\1@[�m@[t�@Z�H@Z�!@Z~�@ZJ@Y��@Y�@Y�#@Y��@Y��@Yx�@Yx�@YG�@X��@X�9@X��@X�@XQ�@X �@W�;@W��@W��@Wl�@W\)@W+@W
=@Vȴ@V5?@U��@U`B@U�@T�@T�@TZ@T(�@T1@S�
@Sƨ@SdZ@SC�@R��@RM�@R�@Q��@Q�#@Q��@Qx�@Q7L@Q%@PbN@O�;@O�@Ol�@O;d@N�y@Nȴ@N��@NE�@N{@M�T@M@M��@M��@M�h@Mp�@MV@L�@L�D@Lj@LI�@K��@K�F@K��@K�@KdZ@K@J��@J=q@JJ@Ihs@I%@H��@H�u@H  @G�@G|�@G;d@G�@Fȴ@Fv�@F5?@E�h@E`B@E?}@Dj@C�m@Cƨ@C��@C"�@Co@B�H@B��@BM�@A�7@Ax�@Ahs@A&�@@��@?�;@?��@?K�@>��@>�R@>ff@>{@=�@=�-@=/@=V@<�/@<Z@;�m@;��@;33@:��@:=q@:-@9��@9�7@9�@8Ĝ@8b@7��@7+@6��@6��@6ff@5��@5`B@4�/@4��@4�@3ƨ@3��@3�@3@2��@2~�@2n�@2=q@1��@1�#@1��@1hs@0��@0�9@0r�@0  @/�w@/�P@/;d@/+@/
=@.��@.E�@-�@-�-@-?}@,��@,�@,I�@+��@+�m@+ƨ@+��@+��@+�@+t�@+C�@*�H@*��@*�\@*-@)�#@)��@)��@)��@)hs@)7L@)�@(�9@(r�@(Q�@(A�@(  @'��@'��@'K�@'�@&�R@&ff@&5?@%��@%��@%�h@%�@%p�@%`B@%`B@%/@$��@$�D@$j@$1@#ƨ@#��@#t�@#dZ@#dZ@#S�@#C�@#"�@"��@"��@"�\@"n�@"�@!��@!�@!x�@!&�@ ��@ Ĝ@ �@ Q�@ A�@ 1'@ b@ b@��@��@|�@\)@K�@K�@+@�y@ȴ@�R@��@5?@�T@@��@O�@��@�/@��@�j@�j@�@��@�D@z�@j@I�@(�@��@ƨ@��@t�@"�@�H@��@��@^5@M�@=q@=q@�@�^@�7@hs@G�@7L@7L@7L@%@�`@bN@  @�w@�P@|�@l�@l�@\)@;d@;d@;d@
=@�y@ȴ@�R@��@E�@@�T@��@@�-@p�@/@V@��@�@�D@z�@Z@�@1@��@��@33@33@"�@�@��@��@�\@n�@^5@-@��@��@hs@7L@&�@�@��@��@�9@�@Q�@1'@  @�;@�@�@�P@l�@K�@;d@�@�@ȴ@��@v�@V@{@111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111B�B� B~�B~�B� B~�B}�B}�B}�B}�B}�B}�B}�B}�B~�B�B�JB�B�jB�B��BB1B	7B
=B
=B	7B	7B1B	7B
=BDBDB	7B
=B�B'�B'�B'�B/B49B2-B49BD�BI�BN�BR�BXB[#BT�BP�BVBQ�BI�BH�BI�BH�BI�BS�BXB[#BZBS�B\)BXBQ�BP�BN�BM�BH�B9XBhB�HBɺB��B��B��B�JB�BhsBK�B49B#�BuBBB �B'�B$�B�BJB
�ZB
ȴB
�FB
��B
��B
�uB
�VB
{�B
iyB
aHB
XB
Q�B
H�B
C�B
<jB
-B
�B
	7B	��B	�sB	�#B	��B	�wB	�B	��B	��B	�\B	�B	n�B	m�B	dZB	YB	S�B	N�B	J�B	C�B	+B	)�B	(�B	%�B	�B	�B	oB	VB	+B	  B��B�B�B�yB�yB�yB�yB�sB�`B�TB�BB�/B�
B��B��B��BɺBǮBĜB�XB�9B�'B�B�B�'B�'B�!B�3B�3B��B��B��B��B��B��B��B�B��B��B��B��B��B��B��B��B��B��B��B��B��B��B�B��B�B�9B�9B�?B�LB�?B�?B�-B�B�B�!B�B�B��B�B�?B�RB�^B�XB�RB�RB�^B�jB�wB�}BBŢBŢB��B��B��B�}B�jB�B��B��B�B�B�B�B�B�XBÖBĜBɺBǮBŢBĜBŢB�wB�dB�RB�dB�}BBÖBĜB��B��BɺBȴBɺB��B��B��B�
B�B�B�B�)B�BB�NB�ZB�mB�yB�B�B��B��B��B	  B	B	%B	
=B	VB	�B	�B	�B	#�B	%�B	(�B	,B	-B	/B	/B	0!B	0!B	0!B	33B	;dB	<jB	<jB	=qB	=qB	A�B	D�B	D�B	E�B	H�B	M�B	P�B	R�B	T�B	W
B	[#B	]/B	`BB	bNB	cTB	dZB	e`B	hsB	m�B	p�B	q�B	q�B	s�B	s�B	t�B	t�B	u�B	x�B	y�B	y�B	y�B	z�B	y�B	w�B	z�B	� B	�B	�bB	�oB	�hB	�hB	�bB	�oB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�-B	�'B	�3B	�LB	�XB	�^B	�^B	�^B	�RB	�FB	�FB	�FB	�LB	�XB	�}B	��B	ǮB	��B	��B	��B	��B	��B	��B	B	ÖB	ƨB	��B	ɺB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�
B	�B	�B	�)B	�/B	�5B	�;B	�HB	�NB	�ZB	�`B	�fB	�fB	�mB	�mB	�mB	�sB	�yB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B
  B
B
B
B
B
B
B
B
B
B
B
%B
+B
1B
1B
	7B

=B

=B
JB
DB
DB
VB
\B
\B
bB
bB
bB
hB
oB
{B
{B
{B
�B
�B
�B
�B
�B
�B
�B
�B
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
�B
�B
�B
�B
 �B
 �B
!�B
!�B
!�B
"�B
"�B
"�B
"�B
#�B
#�B
#�B
$�B
$�B
$�B
%�B
&�B
&�B
&�B
'�B
'�B
'�B
'�B
(�B
(�B
)�B
)�B
)�B
)�B
+B
+B
+B
+B
)�B
+B
,B
-B
-B
-B
.B
.B
.B
/B
/B
/B
0!B
1'B
2-B
33B
2-B
33B
33B
33B
33B
33B
33B
33B
49B
49B
5?B
5?B
5?B
5?B
5?B
5?B
5?B
5?B
5?B
6FB
7LB
6FB
8RB
8RB
8RB
9XB
9XB
9XB
9XB
:^B
;dB
;dB
;dB
<jB
<jB
<jB
=qB
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
?}B
?}B
?}B
?}B
?}B
?}B
@�B
@�B
A�B
A�B
A�B
A�B
A�B
B�B
B�B
B�B
B�B
B�B
B�B
B�B
C�B
C�B
C�B
C�B
C�B
C�B
D�B
D�B
D�B
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
G�B
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
K�B
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
L�B
L�B
L�B
L�B
L�B
L�B
L�B
L�B
M�B
N�B
N�B
N�B
O�B
O�B
O�B
O�B
O�B
P�B
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
Q�B
Q�B
Q�B
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
T�B
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
XB
XB
XB
YB
YB
YB
YB
ZB
ZB
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
]/B
]/B
]/B
]/B
]/B
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
dZB
dZB
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
jB
jB
jB
k�B
k�B
k�B
k�B
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
n�B
o�B
o�B
o�B
o�B
p�B
p�B
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
r�B
r�B
r�B
r�B
s�B
s�B
s�B
t�B
t�B
t�B
t�B
t�B
t�B
t�B
t�B
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
v�B
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
y�B
y�B
y�B
y�B
y�B
y�B
y�B
y�B
y�B
y�B
y�B
y�B
y�B
y�B
y�B
y�B
z�B
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
{�B
{�B
{�B
{�B
|�B
|�B
|�B
|�B
|�B
|�B
}�B
}�B
}�B
}�B
}�B
}�B
}�B
~�B
~�B
~�B
~�B
~�B
~�B
~�B
� B
� B
� B
� B
� B
� B
� B
� B
� B
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
�B
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
�B
�B
�B
�B
�B
�B
�B
�B
�%B
�%B
�%B
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
�1B
�7B
�7B
�7B
�7B
�7B
�7B
�7B
�=111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111B�B� B~�B~�B� B~�B}�B}�B}�B}�B}�B}�B}�B}�B~�B�B�JB�B�jB�B��BB1B	7B
=B
=B	7B	7B1B	7B
=BDBDB	7B
=B�B'�B'�B'�B/B49B2-B49BD�BI�BN�BR�BXB[#BT�BP�BVBQ�BI�BH�BI�BH�BI�BS�BXB[#BZBS�B\)BXBQ�BP�BN�BM�BH�B9XBhB�HBɺB��B��B��B�JB�BhsBK�B49B#�BuBBB �B'�B$�B�BJB
�ZB
ȴB
�FB
��B
��B
�uB
�VB
{�B
iyB
aHB
XB
Q�B
H�B
C�B
<jB
-B
�B
	7B	��B	�sB	�#B	��B	�wB	�B	��B	��B	�\B	�B	n�B	m�B	dZB	YB	S�B	N�B	J�B	C�B	+B	)�B	(�B	%�B	�B	�B	oB	VB	+B	  B��B�B�B�yB�yB�yB�yB�sB�`B�TB�BB�/B�
B��B��B��BɺBǮBĜB�XB�9B�'B�B�B�'B�'B�!B�3B�3B��B��B��B��B��B��B��B�B��B��B��B��B��B��B��B��B��B��B��B��B��B��B�B��B�B�9B�9B�?B�LB�?B�?B�-B�B�B�!B�B�B��B�B�?B�RB�^B�XB�RB�RB�^B�jB�wB�}BBŢBŢB��B��B��B�}B�jB�B��B��B�B�B�B�B�B�XBÖBĜBɺBǮBŢBĜBŢB�wB�dB�RB�dB�}BBÖBĜB��B��BɺBȴBɺB��B��B��B�
B�B�B�B�)B�BB�NB�ZB�mB�yB�B�B��B��B��B	  B	B	%B	
=B	VB	�B	�B	�B	#�B	%�B	(�B	,B	-B	/B	/B	0!B	0!B	0!B	33B	;dB	<jB	<jB	=qB	=qB	A�B	D�B	D�B	E�B	H�B	M�B	P�B	R�B	T�B	W
B	[#B	]/B	`BB	bNB	cTB	dZB	e`B	hsB	m�B	p�B	q�B	q�B	s�B	s�B	t�B	t�B	u�B	x�B	y�B	y�B	y�B	z�B	y�B	w�B	z�B	� B	�B	�bB	�oB	�hB	�hB	�bB	�oB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�-B	�'B	�3B	�LB	�XB	�^B	�^B	�^B	�RB	�FB	�FB	�FB	�LB	�XB	�}B	��B	ǮB	��B	��B	��B	��B	��B	��B	B	ÖB	ƨB	��B	ɺB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�
B	�B	�B	�)B	�/B	�5B	�;B	�HB	�NB	�ZB	�`B	�fB	�fB	�mB	�mB	�mB	�sB	�yB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B
  B
B
B
B
B
B
B
B
B
B
B
%B
+B
1B
1B
	7B

=B

=B
JB
DB
DB
VB
\B
\B
bB
bB
bB
hB
oB
{B
{B
{B
�B
�B
�B
�B
�B
�B
�B
�B
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
�B
�B
�B
�B
 �B
 �B
!�B
!�B
!�B
"�B
"�B
"�B
"�B
#�B
#�B
#�B
$�B
$�B
$�B
%�B
&�B
&�B
&�B
'�B
'�B
'�B
'�B
(�B
(�B
)�B
)�B
)�B
)�B
+B
+B
+B
+B
)�B
+B
,B
-B
-B
-B
.B
.B
.B
/B
/B
/B
0!B
1'B
2-B
33B
2-B
33B
33B
33B
33B
33B
33B
33B
49B
49B
5?B
5?B
5?B
5?B
5?B
5?B
5?B
5?B
5?B
6FB
7LB
6FB
8RB
8RB
8RB
9XB
9XB
9XB
9XB
:^B
;dB
;dB
;dB
<jB
<jB
<jB
=qB
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
?}B
?}B
?}B
?}B
?}B
?}B
@�B
@�B
A�B
A�B
A�B
A�B
A�B
B�B
B�B
B�B
B�B
B�B
B�B
B�B
C�B
C�B
C�B
C�B
C�B
C�B
D�B
D�B
D�B
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
G�B
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
K�B
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
L�B
L�B
L�B
L�B
L�B
L�B
L�B
L�B
M�B
N�B
N�B
N�B
O�B
O�B
O�B
O�B
O�B
P�B
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
Q�B
Q�B
Q�B
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
T�B
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
XB
XB
XB
YB
YB
YB
YB
ZB
ZB
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
]/B
]/B
]/B
]/B
]/B
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
dZB
dZB
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
jB
jB
jB
k�B
k�B
k�B
k�B
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
n�B
o�B
o�B
o�B
o�B
p�B
p�B
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
r�B
r�B
r�B
r�B
s�B
s�B
s�B
t�B
t�B
t�B
t�B
t�B
t�B
t�B
t�B
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
v�B
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
y�B
y�B
y�B
y�B
y�B
y�B
y�B
y�B
y�B
y�B
y�B
y�B
y�B
y�B
y�B
y�B
z�B
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
{�B
{�B
{�B
{�B
|�B
|�B
|�B
|�B
|�B
|�B
}�B
}�B
}�B
}�B
}�B
}�B
}�B
~�B
~�B
~�B
~�B
~�B
~�B
~�B
� B
� B
� B
� B
� B
� B
� B
� B
� B
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
�B
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
�B
�B
�B
�B
�B
�B
�B
�B
�%B
�%B
�%B
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
�1B
�7B
�7B
�7B
�7B
�7B
�7B
�7B
�=111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                            JA  ARFMdecpA19c                                                                20220318184125  IP                  G�O�G�O�G�O�                JA  ARFMfmtp2.8                                                                 20220418145351  IP                  G�O�G�O�G�O�                JA  ARCAsspa3.1b                                                                20220418145352  IP  PRES            G�O�G�O�G�O�                JA  ARGQrqcppo_c                                                                20220418145352  QCP$                G�O�G�O�G�O�              3CJA  ARGQrqcpt19d                                                                20220418145352  QCP$                G�O�G�O�G�O�           80000JA  ARGQaqcpt19d                                                                20220418145352  QCP$                G�O�G�O�G�O�           80000JA  ARGQrqcp2.8e                                                                20220418145352  QCP$                G�O�G�O�G�O�            FB40JA  ARGQaqcp2.8e                                                                20220418145352  QCP$                G�O�G�O�G�O�            FB40JA  ARGQrqcpt16c                                                                20220418145353  QCP$                G�O�G�O�G�O�           10000JA      jafc1.0                                                                 20220418145353                      G�O�G�O�G�O�                JA  ARUP                                                                        20220419211501                      G�O�G�O�G�O�                