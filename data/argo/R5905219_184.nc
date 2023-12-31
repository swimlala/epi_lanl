CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       JMA    source        
Argo float     history       E2023-04-12T18:42:27Z creation;2023-04-12T18:42:29Z conversion to V3.1      
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
_FillValue                 �  I   PRES_ADJUSTED            
      	   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  M   PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  \�   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     
_FillValue        G�O�   	valid_min         �      	valid_max         B      C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  `�   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  p$   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     
_FillValue        G�O�   	valid_min         �      	valid_max         B      C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  t   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ��   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    
_FillValue        G�O�   	valid_min         @      	valid_max         B$     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  ��   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �,   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    
_FillValue        G�O�   	valid_min         @      	valid_max         B$     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  �   PSAL_ADJUSTED_QC         
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
_FillValue                  0  �l   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                    ݜ   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                    ��   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                    �   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  ,  �   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                    ��   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                    ��   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                    ��   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                    ��   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  @  ��   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                    �   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                    �(   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    �,   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar         �<   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar         �@   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�        �D   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    �HArgo profile    3.1 1.2 19500101000000  20230412184227  20230412185655  5905219                                                                 JAMSTEC                                                         PRES            TEMP            PSAL               �A   JA                                  2B  A   APEX                            7906                            051216                          846 @�#%�@1   @�#&Y <�@5XQ���cݺ^5?}1   GPS     A   A   F   Primary sampling: averaged [2 dbar bin average for 1 Hz CTD]                                                                                                                                                                                                       @�ff@�  @���AffA@  A`  A�  A�  A�  A�  A�  A�  A�  A�  A�33B  B  B  B   B(  B0  B8  B@  BH  BP  BX  B`  Bh  Bp  Bx  B�  B�  B�  B���B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�33B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�33B�33B�  C   C  C  C  C  C
  C  C  C  C�C  C  C  C  C  C  C�fC"  C$  C&�C(�C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C]�fC`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C��3C�  C�  C�  C�  C�  C��3C��3C�  C�  C�  C�  C�  C�  C�  C��3C�  C�  C�  C�  C�  C��3C�  C�  C�  C��3C��3C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D�fDfD� D  D� D  D� D��D	� D	��D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D��D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&y�D'  D'� D(  D(� D)fD)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D/��D0� D1  D1� D2  D2�fD3  D3� D4fD4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DG��DHy�DI  DI� DI��DJ� DK  DK� DLfDL�fDM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DX��DY� DZ  DZ� D[  D[� D\  D\� D]  D]y�D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dgy�Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dly�Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr�fDs  Ds� Dt  Dt� Du  Du� Dv  Dv� Dw  Dw� Dx  Dx� Dy  Dy� Dz  Dz� D{  D{� D|  D|� D}  D}� D~  D~� D  D� D�  D�@ D�|�D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�C3D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D��3D�  D�@ D�� D���D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�C3D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�3D�@ D�|�D�� D�  D�@ D�� D�� D�  D�@ D��3D��3D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�C3D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�|�D���D�  D�@ D�� D�� D���D�@ D�� D�� D�  D�<�D�|�D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D��3D�� D�  D�<�D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D D�� D�  D�@ DÀ D�� D���D�<�DĀ D�� D�  D�@ Dŀ D�� D�  D�@ Dƀ D�� D�  D�@ Dǀ D�� D�3D�@ DȀ D�� D�  D�@ Dɀ D�� D�  D�@ Dʀ D�� D�  D�C3Dˀ D�� D�  D�@ D̀ D�� D�  D�@ D̀ D�� D�  D�@ D΀ D�� D�  D�@ Dπ D��3D�  D�@ DЀ D�� D�  D�@ Dр D�� D���D�@ DҀ D�� D�  D�@ DӀ D�� D�  D�@ DԀ D�� D�  D�@ DՀ Dռ�D�  D�@ Dր D�� D�  D�@ D׀ D�� D�  D�@ D؀ D��3D�  D�@ Dـ D�� D�  D�@ Dڀ D�� D�  D�@ Dۀ D�� D�  D�@ D܀ D�� D�  D�@ D݀ D�� D�  D�@ Dހ D�� D�  D�@ D߀ D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D��3D�3D�@ D�3D�� D�  D�<�D�|�D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�3D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D�|�D�� D�  D�@ D�|�D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D��3D���111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 @�p�@�
=A�A%�AG�Ag�A�A�A�A�A�A�A�A�Bz�B	�HB�HB�HB!�HB)�HB1�HB9�HBA�HBI�HBQ�HBY�HBa�HBi�HBq�HBy�HB��B��B��B��qB��B��B��B��B��B��B��B��B��B��B��B��B��B��B�#�B��B��B��B��B��B��B��B��B��B��B�#�B�#�B��C xRCxRCxRCxRCxRC
xRCxRCxRCxRC��CxRCxRCxRCxRCxRCxRC ^�C"xRC$xRC&��C(��C*xRC,xRC.xRC0xRC2xRC4xRC6xRC8xRC:xRC<xRC>xRC@xRCBxRCDxRCFxRCHxRCJxRCLxRCNxRCPxRCRxRCTxRCVxRCXxRCZxRC\xRC^^�C`xRCbxRCdxRCfxRChxRCjxRClxRCnxRCpxRCrxRCtxRCvxRCxxRCzxRC|xRC~xRC�<)C�<)C�<)C�<)C�<)C�<)C�<)C�<)C�<)C�<)C�<)C�<)C�<)C�<)C�<)C�<)C�<)C�<)C�H�C�<)C�<)C�<)C�<)C�<)C�<)C�<)C�<)C�<)C�<)C�<)C�<)C�<)C�<)C�<)C�<)C�<)C�<)C�<)C�<)C�<)C�<)C�<)C�<)C�/\C�/\C�<)C�<)C�<)C�<)C�<)C�/\C�/\C�<)C�<)C�<)C�<)C�<)C�<)C�<)C�/\C�<)C�<)C�<)C�<)C�<)C�/\C�<)C�<)C�<)C�/\C�/\C�<)C�<)C�<)C�<)C�H�C�<)C�<)C�<)C�<)C�<)C�<)C�<)C�<)C�<)C�<)C�<)C�<)C�H�C�<)C�<)C�<)C�<)C�<)C�<)C�<)C�<)C�<)C�<)C�<)C�<)C�<)C�<)C�<)C�<)C�<)C�<)C�<)C�<)C�<)C�<)C�<)C�<)C�<)C�<)C�<)C�<)C�<)C�<)C�<)C�<)C�<)C�H�C�<)C�<)C�<)C�<)C�<)D D �DD�DD�DD�DD�DD�zD$zD�DD�DD�D	�D	�D
�D
�DD�DD�DD�DD�DD�DD�DD�DD�DD�DD�DD�DD�DD�DD�D�D�DD�DD�DD�DD�DD�DD�D D �D!D!�D"D"�D#D#�D$D$�D%D%�D&D&��D'D'�D(D(�D)$zD)�D*D*�D+D+�D,D,�D-D-�D.D.�D/D/�D0�D0�D1D1�D2D2�zD3D3�D4$zD4�D5D5�D6D6�D7D7�D8D8�D9D9�D:D:�D;D;�D<D<�D=D=�D>D>�D?D?�D@D@�DADA�DBDB�DCDC�DDDD�DEDE�DFDF�DGDG�DH�DH��DIDI�DJ�DJ�DKDK�DL$zDL�zDMDM�DNDN�DODO�DPDP�DQDQ�DRDR�DSDS�DTDT�DUDU�DVDV�DWDW�DXDX�DY�DY�DZDZ�D[D[�D\D\�D]D]��D^D^�D_D_�D`D`�DaDa�DbDb�DcDc�DdDd�DeDe�DfDf�DgDg��DhDh�DiDi�DjDj�DkDk�DlDl��DmDm�DnDn�DoDo�DpDp�DqDq�DrDr�zDsDs�DtDt�DuDu�DvDv�DwDw�DxDx�DyDy�DzDz�D{D{�D|D|�D}D}�D~D~�DD�D�
D�O
D���D��
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
D��=D�
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
D�=D�O
D���D��
D�
D�O
D��
D��
D�
D�O
D��=D��=D�
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
D���D���D�
D�O
D��
D��
D��D�O
D��
D��
D�
D�K�D���D��
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
D�K�D��
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
D
D��
D�
D�O
DÏ
D��
D��D�K�Dď
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
D�=D�O
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
D�R=Dˏ
D��
D�
D�O
D̏
D��
D�
D�O
D͏
D��
D�
D�O
DΏ
D��
D�
D�O
DϏ
D��=D�
D�O
DЏ
D��
D�
D�O
Dя
D��
D��D�O
Dҏ
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
D���D�
D�O
D֏
D��
D�
D�O
D׏
D��
D�
D�O
D؏
D��=D�
D�O
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
D��=D�=D�O
D�=D��
D�
D�K�D��D��
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
D��
D�=D�O
D��
D��
D�
D�O
D�
D��
D�
D�O
D��D��
D�
D�O
D��D��
D�
D�O
D�
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
D��=D���111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 A�%A�bA��A��A��A��A��A��A��A��A��A��A��A���A�l�A�hsA���A��jA�dZA���A��9A��+A��PA���A�33A���A�|�A�M�A�&�A��A��A���A�ȴA�bNA�S�A�dZA�?}A���A���A�XA�z�A�z�A�E�A�C�A�9XA���A�z�A�9XA���A�hsA�
=A� �A��A��!A��A�O�A��;A�I�A��DA�^5A�;dA���A���A�E�A�ƨA�$�A���A�VA�{A���A�|�A���A�?}A�$�A���A��A�bA��wA��A�`BA�-A�?}A�`BA���A|-Ay�TAx  Atz�Ap�An��Ak�AhȴAe�mAb�yAbn�A_��A[C�AV�HAUVAT�jATI�AR�!AR  AQ��AQ��AO�#AKdZAG�AD^5AAt�A=�A<�!A<ffA<{A;��A:5?A7hsA3�A1��A0�9A/��A/K�A.�RA-�A,(�A*��A*(�A*A)��A(M�A'�A%�A$(�A#�A!�#A �uA�AO�A33A/A/A/A"�A�A�^A��A|�AoAr�AJA��Ap�AXAI�A&�A�RA�wAM�A�A�`AjA  AK�A��A
�RA	ƨAbNA�TA��AdZA&�A�A��A�A/A��A�uAr�AJA\)A+A {@��@�p�@�G�@��@�Q�@��T@��u@���@�n�@�?}@��@�V@��/@�z�@�b@��@��@��@�E�@��@���@�7L@睲@�M�@���@�9X@�;d@�Q�@�A�@٩�@���@��H@���@�O�@�I�@ӍP@�"�@�@�z�@ΰ!@͡�@�(�@��@ʗ�@�$�@�-@�b@̼j@��@��m@��
@ˮ@�K�@ʇ+@�M�@��#@�p�@�`B@�7L@���@ț�@�Q�@�t�@��@ư!@�-@��#@ũ�@�hs@�%@ě�@î@+@��@�@��^@�@�@��-@���@��h@�hs@�A�@��w@�"�@���@���@��@�?}@�V@��`@��@�Q�@�A�@� �@��@�b@�  @���@�33@�M�@�p�@���@��@��;@���@�l�@�K�@�+@�"�@��@�M�@���@��@�G�@�/@��@�V@���@��9@�bN@� �@��@�33@�$�@���@�`B@�O�@�7L@�&�@�V@��`@�Ĝ@���@�z�@�bN@�Z@�9X@��@�1@�  @�  @��m@�ƨ@��@�S�@�
=@���@�-@�{@���@��^@��h@�x�@�7L@��@�r�@�9X@�  @��;@��@�;d@��@��y@���@�V@�{@��-@�x�@�G�@�7L@��@���@��@�9X@� �@�b@�  @��@��@�33@��y@��!@��\@�$�@���@�O�@��j@�j@�1'@��@��w@�K�@��^@�x�@�O�@�/@��@���@��`@�Ĝ@��D@�1'@���@�33@��@�~�@�=q@�5?@���@�hs@�V@��j@��D@�9X@�b@�|�@�K�@�
=@���@��y@��R@��\@�v�@�=q@�@�@��-@���@��7@�?}@��`@��@� �@�  @��
@���@��P@�;d@��@���@���@��+@�ff@�5?@�G�@� �@��F@�|�@�+@���@���@�ff@�J@��#@���@�O�@���@�bN@�9X@�b@��@���@��@�dZ@���@���@��\@�n�@�ff@�^5@�J@��@�hs@�?}@��u@���@��@�t�@�S�@�"�@��H@���@�E�@��@��@�7L@��`@���@�Z@�(�@�b@�1@��m@���@�K�@�@��y@��@�ȴ@�v�@�$�@��T@���@�`B@�%@���@���@��`@��@��@�Q�@�1'@�(�@�1@��m@��w@�dZ@�K�@�C�@�33@��@���@��@���@�n�@�E�@��@���@���@�G�@�7L@��@�V@�%@��@���@���@�r�@�z�@�9X@� �@�b@�w@�P@l�@l�@l�@l�@\)@~��@~�+@~5?@}�-@}V@|��@|��@|z�@|�@{�F@{��@{@z~�@zn�@zM�@y�7@yG�@y7L@y%@xĜ@xb@w�P@wl�@v�y@vv�@v@u�h@u�@u�@u�@uO�@t��@t�@s�F@r��@r~�@rn�@rM�@qx�@p�u@pA�@p1'@p  @o�w@o�P@oK�@n�@n�+@m�T@m`B@m/@l��@l�j@lj@k��@k�@kC�@j�!@i��@ix�@h�@h  @g�w@g|�@gK�@g+@f�+@f{@f@e�-@eV@d�@d��@d�D@d�@c�F@cS�@co@b~�@bM�@b-@b-@a��@a7L@`1'@_l�@_+@_
=@^�y@^ȴ@^�+@^{@]��@]/@\��@\�D@\1@[�F@[t�@[33@Z�@Z��@Zn�@Z=q@Y�@Yhs@Y7L@XĜ@X �@Xb@X  @W�@W�;@W��@Wl�@W
=@Vv�@V{@U�-@U�h@UO�@UV@T�@T�j@T�@T�@T9X@SC�@So@R�@R��@R=q@R�@Q�#@Q�^@Q��@Q�7@QX@Q7L@P�`@PĜ@Pr�@P  @O�P@O�@Nȴ@N��@NV@N{@M�T@M`B@L�/@Lj@L1@Kƨ@Kt�@K"�@J��@J�\@J=q@I��@I�@Hr�@G��@F�+@E�@E?}@D�@D�j@D�D@DZ@D(�@D�@D1@Cƨ@B�H@B=q@A�#@Ax�@AG�@A&�@A�@A�@A�@A%@@��@@��@@Ĝ@@�9@@�u@@A�@?�;@?�P@?l�@>$�@=��@=�@=O�@<j@;o@:��@:=q@9�#@9��@9��@9�7@9x�@9X@9&�@9%@8��@8��@8Q�@8 �@8  @7|�@6�y@6�+@6v�@6v�@6ff@6ff@6ff@6ff@6v�@6ff@6E�@65?@6@5��@5�@5�@4��@4��@4�@4�/@4z�@4(�@4�@41@3�
@3��@3��@3�@3t�@3S�@3o@2��@2��@2�!@2J@1��@1��@1x�@1X@0��@0 �@/��@/�w@/�P@/\)@/;d@/;d@/�@/
=@.��@.v�@.V@.5?@.5?@.$�@.$�@-�h@-O�@-V@,��@,��@+��@+��@+t�@+33@+33@+@*��@*��@*^5@*�@)�#@)&�@(��@(��@(��@(��@(�9@(�9@(�9@(�9@(�9@(�9@(bN@'�w@&��@&$�@%�@%�-@%p�@%O�@%/@%V@$�D@$9X@$1@#�F@#��@#33@"�@"��@"��@"~�@!��@ ��@ �@�@�w@�@�P@\)@+@��@��@V@$�@@�@��@�@V@�@��@j@I�@(�@��@�F@��@�@�@dZ@@~�@M�@x�@�u@ �@b@  @  @��@�@|�@\)@+@�@
=@��@�+@v�@{@�@��@`B@/@��@�@I�@1@�
@��@��@t�@dZ@S�@�H@~�@^5@��@X@%@�`@��@��@Ĝ@r�@1'@�@�@�;@�P@K�@�@��@�@�R@�R@��@��@��@�+@v�@E�@5?@$�@{@{@{@{@@@�T@�T@�-@p�@`B@?}@�@��@z�@Z@9X@1@�m@ƨ@��@t�@S�@C�@33@o@@
�H@
��@
�\@
n�@
M�@
-@
J@	�@	�#@	x�@�`@��@�u@�u@�@�@r�@r�@A�@ �@�@�P@K�@ȴ@v�@5?@@111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 A�%A�bA��A��A��A��A��A��A��A��A��A��A��A���A�l�A�hsA���A��jA�dZA���A��9A��+A��PA���A�33A���A�|�A�M�A�&�A��A��A���A�ȴA�bNA�S�A�dZA�?}A���A���A�XA�z�A�z�A�E�A�C�A�9XA���A�z�A�9XA���A�hsA�
=A� �A��A��!A��A�O�A��;A�I�A��DA�^5A�;dA���A���A�E�A�ƨA�$�A���A�VA�{A���A�|�A���A�?}A�$�A���A��A�bA��wA��A�`BA�-A�?}A�`BA���A|-Ay�TAx  Atz�Ap�An��Ak�AhȴAe�mAb�yAbn�A_��A[C�AV�HAUVAT�jATI�AR�!AR  AQ��AQ��AO�#AKdZAG�AD^5AAt�A=�A<�!A<ffA<{A;��A:5?A7hsA3�A1��A0�9A/��A/K�A.�RA-�A,(�A*��A*(�A*A)��A(M�A'�A%�A$(�A#�A!�#A �uA�AO�A33A/A/A/A"�A�A�^A��A|�AoAr�AJA��Ap�AXAI�A&�A�RA�wAM�A�A�`AjA  AK�A��A
�RA	ƨAbNA�TA��AdZA&�A�A��A�A/A��A�uAr�AJA\)A+A {@��@�p�@�G�@��@�Q�@��T@��u@���@�n�@�?}@��@�V@��/@�z�@�b@��@��@��@�E�@��@���@�7L@睲@�M�@���@�9X@�;d@�Q�@�A�@٩�@���@��H@���@�O�@�I�@ӍP@�"�@�@�z�@ΰ!@͡�@�(�@��@ʗ�@�$�@�-@�b@̼j@��@��m@��
@ˮ@�K�@ʇ+@�M�@��#@�p�@�`B@�7L@���@ț�@�Q�@�t�@��@ư!@�-@��#@ũ�@�hs@�%@ě�@î@+@��@�@��^@�@�@��-@���@��h@�hs@�A�@��w@�"�@���@���@��@�?}@�V@��`@��@�Q�@�A�@� �@��@�b@�  @���@�33@�M�@�p�@���@��@��;@���@�l�@�K�@�+@�"�@��@�M�@���@��@�G�@�/@��@�V@���@��9@�bN@� �@��@�33@�$�@���@�`B@�O�@�7L@�&�@�V@��`@�Ĝ@���@�z�@�bN@�Z@�9X@��@�1@�  @�  @��m@�ƨ@��@�S�@�
=@���@�-@�{@���@��^@��h@�x�@�7L@��@�r�@�9X@�  @��;@��@�;d@��@��y@���@�V@�{@��-@�x�@�G�@�7L@��@���@��@�9X@� �@�b@�  @��@��@�33@��y@��!@��\@�$�@���@�O�@��j@�j@�1'@��@��w@�K�@��^@�x�@�O�@�/@��@���@��`@�Ĝ@��D@�1'@���@�33@��@�~�@�=q@�5?@���@�hs@�V@��j@��D@�9X@�b@�|�@�K�@�
=@���@��y@��R@��\@�v�@�=q@�@�@��-@���@��7@�?}@��`@��@� �@�  @��
@���@��P@�;d@��@���@���@��+@�ff@�5?@�G�@� �@��F@�|�@�+@���@���@�ff@�J@��#@���@�O�@���@�bN@�9X@�b@��@���@��@�dZ@���@���@��\@�n�@�ff@�^5@�J@��@�hs@�?}@��u@���@��@�t�@�S�@�"�@��H@���@�E�@��@��@�7L@��`@���@�Z@�(�@�b@�1@��m@���@�K�@�@��y@��@�ȴ@�v�@�$�@��T@���@�`B@�%@���@���@��`@��@��@�Q�@�1'@�(�@�1@��m@��w@�dZ@�K�@�C�@�33@��@���@��@���@�n�@�E�@��@���@���@�G�@�7L@��@�V@�%@��@���@���@�r�@�z�@�9X@� �@�b@�w@�P@l�@l�@l�@l�@\)@~��@~�+@~5?@}�-@}V@|��@|��@|z�@|�@{�F@{��@{@z~�@zn�@zM�@y�7@yG�@y7L@y%@xĜ@xb@w�P@wl�@v�y@vv�@v@u�h@u�@u�@u�@uO�@t��@t�@s�F@r��@r~�@rn�@rM�@qx�@p�u@pA�@p1'@p  @o�w@o�P@oK�@n�@n�+@m�T@m`B@m/@l��@l�j@lj@k��@k�@kC�@j�!@i��@ix�@h�@h  @g�w@g|�@gK�@g+@f�+@f{@f@e�-@eV@d�@d��@d�D@d�@c�F@cS�@co@b~�@bM�@b-@b-@a��@a7L@`1'@_l�@_+@_
=@^�y@^ȴ@^�+@^{@]��@]/@\��@\�D@\1@[�F@[t�@[33@Z�@Z��@Zn�@Z=q@Y�@Yhs@Y7L@XĜ@X �@Xb@X  @W�@W�;@W��@Wl�@W
=@Vv�@V{@U�-@U�h@UO�@UV@T�@T�j@T�@T�@T9X@SC�@So@R�@R��@R=q@R�@Q�#@Q�^@Q��@Q�7@QX@Q7L@P�`@PĜ@Pr�@P  @O�P@O�@Nȴ@N��@NV@N{@M�T@M`B@L�/@Lj@L1@Kƨ@Kt�@K"�@J��@J�\@J=q@I��@I�@Hr�@G��@F�+@E�@E?}@D�@D�j@D�D@DZ@D(�@D�@D1@Cƨ@B�H@B=q@A�#@Ax�@AG�@A&�@A�@A�@A�@A%@@��@@��@@Ĝ@@�9@@�u@@A�@?�;@?�P@?l�@>$�@=��@=�@=O�@<j@;o@:��@:=q@9�#@9��@9��@9�7@9x�@9X@9&�@9%@8��@8��@8Q�@8 �@8  @7|�@6�y@6�+@6v�@6v�@6ff@6ff@6ff@6ff@6v�@6ff@6E�@65?@6@5��@5�@5�@4��@4��@4�@4�/@4z�@4(�@4�@41@3�
@3��@3��@3�@3t�@3S�@3o@2��@2��@2�!@2J@1��@1��@1x�@1X@0��@0 �@/��@/�w@/�P@/\)@/;d@/;d@/�@/
=@.��@.v�@.V@.5?@.5?@.$�@.$�@-�h@-O�@-V@,��@,��@+��@+��@+t�@+33@+33@+@*��@*��@*^5@*�@)�#@)&�@(��@(��@(��@(��@(�9@(�9@(�9@(�9@(�9@(�9@(bN@'�w@&��@&$�@%�@%�-@%p�@%O�@%/@%V@$�D@$9X@$1@#�F@#��@#33@"�@"��@"��@"~�@!��@ ��@ �@�@�w@�@�P@\)@+@��@��@V@$�@@�@��@�@V@�@��@j@I�@(�@��@�F@��@�@�@dZ@@~�@M�@x�@�u@ �@b@  @  @��@�@|�@\)@+@�@
=@��@�+@v�@{@�@��@`B@/@��@�@I�@1@�
@��@��@t�@dZ@S�@�H@~�@^5@��@X@%@�`@��@��@Ĝ@r�@1'@�@�@�;@�P@K�@�@��@�@�R@�R@��@��@��@�+@v�@E�@5?@$�@{@{@{@{@@@�T@�T@�-@p�@`B@?}@�@��@z�@Z@9X@1@�m@ƨ@��@t�@S�@C�@33@o@@
�H@
��@
�\@
n�@
M�@
-@
J@	�@	�#@	x�@�`@��@�u@�u@�@�@r�@r�@A�@ �@�@�P@K�@ȴ@v�@5?@@111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 BW
BW
BW
BW
BW
BW
BW
BW
BW
BW
BW
BW
BVBXB[#B]/B`BBaHB`BB`BB`BBcTBffBcTBo�BhsBe`BdZBdZBcTBbNBbNB`BB_;B^5B\)BZBVBT�BO�BH�B@�B/B"�B�BuBVBJB	7BB��B��B�mB�TB�#B�B��B�XB�B��B��B�Be`BN�B5?B+B#�B�BDB
��B
�B
�mB
�;B
�/B
�B
��B
�}B
�^B
�9B
��B
|�B
aHB
O�B
G�B
$�B
hB
B	�B	��B	ŢB	�-B	��B	��B	~�B	z�B	r�B	`BB	F�B	=qB	9XB	7LB	2-B	+B	(�B	&�B	 �B	DB��B�B�BB��B��B��BɺBǮBÖB�jB�'B��B��B��B��B��B��B��B��B��B��B��B��B�hB�oB�PB�PB�JB�DB�=B�=B�7B�7B�7B�1B�B�JB�+B�1B�+B�%B�%B�%B�%B�B�B�B�%B�B�=B�\B�bB�hB�hB�\B�VB�JB�1B�%B�B�B�7B�DB�DB�JB�7B�1B�%B�%B�%B�+B�7B�=B�7B�DB�7B�1B�+B�+B�+B�+B�%B�+B�B�+B�=B�hB��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B�B�3B�XB�}BɺB��B��B��B�
B�B�#B�sB��B��B	  B	  B	B	B	B	B	+B		7B	
=B	DB	JB	VB	\B	uB	�B	�B	�B	�B	�B	�B	 �B	"�B	'�B	-B	.B	/B	0!B	/B	0!B	0!B	2-B	2-B	49B	<jB	=qB	A�B	G�B	I�B	I�B	J�B	K�B	L�B	M�B	O�B	O�B	O�B	P�B	P�B	P�B	Q�B	S�B	YB	^5B	cTB	ffB	gmB	hsB	iyB	iyB	jB	jB	k�B	o�B	q�B	r�B	s�B	t�B	t�B	t�B	u�B	v�B	x�B	x�B	y�B	|�B	�B	�B	�%B	�+B	�+B	�1B	�7B	�=B	�DB	�JB	�PB	�VB	�VB	�\B	�\B	�bB	�bB	�bB	�bB	�bB	�hB	�oB	�{B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�!B	�'B	�3B	�9B	�9B	�?B	�?B	�FB	�RB	�XB	�XB	�^B	�^B	�^B	�dB	�qB	�wB	�}B	��B	B	ĜB	ƨB	ȴB	ɺB	��B	��B	��B	��B	�
B	�
B	�B	�B	�B	�B	�B	�B	�)B	�)B	�5B	�BB	�NB	�TB	�ZB	�ZB	�fB	�sB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
B
B
B
B
%B
+B
+B
1B
1B
1B
1B
	7B
JB
PB
PB
PB
VB
VB
VB
\B
bB
bB
bB
bB
bB
bB
hB
oB
oB
oB
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
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
"�B
"�B
"�B
#�B
$�B
%�B
%�B
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
)�B
+B
+B
+B
,B
,B
,B
,B
,B
-B
-B
-B
.B
/B
/B
/B
0!B
0!B
0!B
0!B
0!B
0!B
1'B
1'B
1'B
1'B
1'B
2-B
2-B
2-B
2-B
2-B
2-B
2-B
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
7LB
7LB
7LB
7LB
8RB
8RB
8RB
8RB
8RB
9XB
:^B
9XB
:^B
:^B
;dB
;dB
;dB
;dB
;dB
;dB
;dB
<jB
<jB
=qB
=qB
=qB
=qB
>wB
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
B�B
B�B
B�B
B�B
C�B
C�B
C�B
C�B
D�B
D�B
E�B
E�B
F�B
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
J�B
J�B
K�B
J�B
J�B
J�B
K�B
L�B
M�B
M�B
M�B
N�B
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
ZB
ZB
ZB
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
bNB
bNB
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
ffB
ffB
ffB
ffB
ffB
gmB
gmB
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
iyB
iyB
iyB
iyB
iyB
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
l�B
m�B
m�B
m�B
m�B
n�B
o�B
o�B
o�B
o�B
o�B
o�B
o�B
o�B
o�B
o�B
o�B
o�B
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
t�B
u�B
u�B
v�B
v�B
v�B
v�B
v�B
v�B
w�B
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
|�B
|�B
|�B
|�B
|�B
|�B
|�B
|�B
|�B
{�B
|�B
|�B
}�B
~�B
~�B
� B
� B
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
�B
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
�+B
�+B
�+B
�+B
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
�=B
�=B
�DB
�JB
�PB
�PB
�PB
�PB
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
�\B
�\B
�\B
�\B
�bB
�bB
�bB
�bB
�hB
�hB
�hB
�hB
�hB
�hB
�hB
�oB
�oB
�oB
�uB
�uB
�uB
�uB
�uB
�uB
�uB
�{B
�{B
��B
�{B
�{B
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
��B
��B
��B
��B
��B
��333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333 BW
BW
BW
BW
BW
BW
BW
BW
BW
BW
BW
BW
BVBXB[#B]/B`BBaHB`BB`BB`BBcTBffBcTBo�BhsBe`BdZBdZBcTBbNBbNB`BB_;B^5B\)BZBVBT�BO�BH�B@�B/B"�B�BuBVBJB	7BB��B��B�mB�TB�#B�B��B�XB�B��B��B�Be`BN�B5?B+B#�B�BDB
��B
�B
�mB
�;B
�/B
�B
��B
�}B
�^B
�9B
��B
|�B
aHB
O�B
G�B
$�B
hB
B	�B	��B	ŢB	�-B	��B	��B	~�B	z�B	r�B	`BB	F�B	=qB	9XB	7LB	2-B	+B	(�B	&�B	 �B	DB��B�B�BB��B��B��BɺBǮBÖB�jB�'B��B��B��B��B��B��B��B��B��B��B��B��B�hB�oB�PB�PB�JB�DB�=B�=B�7B�7B�7B�1B�B�JB�+B�1B�+B�%B�%B�%B�%B�B�B�B�%B�B�=B�\B�bB�hB�hB�\B�VB�JB�1B�%B�B�B�7B�DB�DB�JB�7B�1B�%B�%B�%B�+B�7B�=B�7B�DB�7B�1B�+B�+B�+B�+B�%B�+B�B�+B�=B�hB��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B�B�3B�XB�}BɺB��B��B��B�
B�B�#B�sB��B��B	  B	  B	B	B	B	B	+B		7B	
=B	DB	JB	VB	\B	uB	�B	�B	�B	�B	�B	�B	 �B	"�B	'�B	-B	.B	/B	0!B	/B	0!B	0!B	2-B	2-B	49B	<jB	=qB	A�B	G�B	I�B	I�B	J�B	K�B	L�B	M�B	O�B	O�B	O�B	P�B	P�B	P�B	Q�B	S�B	YB	^5B	cTB	ffB	gmB	hsB	iyB	iyB	jB	jB	k�B	o�B	q�B	r�B	s�B	t�B	t�B	t�B	u�B	v�B	x�B	x�B	y�B	|�B	�B	�B	�%B	�+B	�+B	�1B	�7B	�=B	�DB	�JB	�PB	�VB	�VB	�\B	�\B	�bB	�bB	�bB	�bB	�bB	�hB	�oB	�{B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�!B	�'B	�3B	�9B	�9B	�?B	�?B	�FB	�RB	�XB	�XB	�^B	�^B	�^B	�dB	�qB	�wB	�}B	��B	B	ĜB	ƨB	ȴB	ɺB	��B	��B	��B	��B	�
B	�
B	�B	�B	�B	�B	�B	�B	�)B	�)B	�5B	�BB	�NB	�TB	�ZB	�ZB	�fB	�sB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
B
B
B
B
%B
+B
+B
1B
1B
1B
1B
	7B
JB
PB
PB
PB
VB
VB
VB
\B
bB
bB
bB
bB
bB
bB
hB
oB
oB
oB
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
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
"�B
"�B
"�B
#�B
$�B
%�B
%�B
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
)�B
+B
+B
+B
,B
,B
,B
,B
,B
-B
-B
-B
.B
/B
/B
/B
0!B
0!B
0!B
0!B
0!B
0!B
1'B
1'B
1'B
1'B
1'B
2-B
2-B
2-B
2-B
2-B
2-B
2-B
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
7LB
7LB
7LB
7LB
8RB
8RB
8RB
8RB
8RB
9XB
:^B
9XB
:^B
:^B
;dB
;dB
;dB
;dB
;dB
;dB
;dB
<jB
<jB
=qB
=qB
=qB
=qB
>wB
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
B�B
B�B
B�B
B�B
C�B
C�B
C�B
C�B
D�B
D�B
E�B
E�B
F�B
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
J�B
J�B
K�B
J�B
J�B
J�B
K�B
L�B
M�B
M�B
M�B
N�B
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
ZB
ZB
ZB
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
bNB
bNB
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
ffB
ffB
ffB
ffB
ffB
gmB
gmB
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
iyB
iyB
iyB
iyB
iyB
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
l�B
m�B
m�B
m�B
m�B
n�B
o�B
o�B
o�B
o�B
o�B
o�B
o�B
o�B
o�B
o�B
o�B
o�B
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
t�B
u�B
u�B
v�B
v�B
v�B
v�B
v�B
v�B
w�B
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
|�B
|�B
|�B
|�B
|�B
|�B
|�B
|�B
|�B
{�B
|�B
|�B
}�B
~�B
~�B
� B
� B
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
�B
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
�+B
�+B
�+B
�+B
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
�=B
�=B
�DB
�JB
�PB
�PB
�PB
�PB
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
�\B
�\B
�\B
�\B
�bB
�bB
�bB
�bB
�hB
�hB
�hB
�hB
�hB
�hB
�hB
�oB
�oB
�oB
�uB
�uB
�uB
�uB
�uB
�uB
�uB
�{B
�{B
��B
�{B
�{B
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
��B
��B
��B
��B
��B
��333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333 G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                            JA  ARFMdecpA19c                                                                20230413034208  IP                  G�O�G�O�G�O�                JA  ARFMfmtp2.8b                                                                20230412184227  IP                  G�O�G�O�G�O�                JA  ARCAsspa3.1b                                                                20230412184228  IP  PRES            G�O�G�O�G�O�                JA      jafc1.0                                                                 20230412184229                      G�O�G�O�G�O�                JA  ARGQrqcpc3.6                                                                20230412184230  QCP$                G�O�G�O�G�O�         20DF37EJA  ARGQrqcpc3.6                                                                20230412184230  QCF$                G�O�G�O�G�O�            8000JA  ARUP                                                                        20230412185655                      G�O�G�O�G�O�                