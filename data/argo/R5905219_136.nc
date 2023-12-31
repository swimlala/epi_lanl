CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       JMA    source        
Argo float     history       E2022-01-04T15:43:44Z creation;2022-01-04T15:43:46Z conversion to V3.1      
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
_FillValue                 �  I   PRES_ADJUSTED            
      	   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  L�   PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  \�   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     
_FillValue        G�O�   	valid_min         �      	valid_max         B      C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  `x   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  p   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     
_FillValue        G�O�   	valid_min         �      	valid_max         B      C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  s�   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ��   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    
_FillValue        G�O�   	valid_min         @      	valid_max         B$     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  �p   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    
_FillValue        G�O�   	valid_min         @      	valid_max         B$     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  ��   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ��   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         decibar    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  �h   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         degree_Celsius     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  ��   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  ͐   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  0  �$   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                    �T   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                    �T   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                    �T   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  ,  �T   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                    �   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                    �   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                    �   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                    �   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  @  �   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                    ��   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                    ��   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    ��   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar         ��   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar         ��   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�        ��   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    � Argo profile    3.1 1.2 19500101000000  20220104154344  20220104155208  5905219                                                                 JAMSTEC                                                         PRES            TEMP            PSAL               �A   JA                                  2B  A   APEX                            7906                            051216                          846 @٭P,�W1   @٭P����@4�n��P�dW\(�1   GPS     A   A   A   Primary sampling: averaged [2 dbar bin average for 1 Hz CTD]                                                                                                                                                                                                       @�ff@�  @���A   A@  A`  A�  A�  A�  A�33A�33A�  A�  A�  B   B  B  B  B   B(  B0  B8  B@  BG��BP  BX  B`  Bh  Bp  Bw��B�  B�33B�  B�33B�33B�  B�  B�  B�  B�  B�  B�  B�33B�33B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�33B�  B�  B�  B�  B�  C   C  C  C  C  C
  C  C  C�fC  C  C  C  C  C  C  C   C"  C$  C&  C'�fC*  C+�fC.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL�CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C��3C�  C��3C��3C��3C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C�  C��C�  C�  C�  C�  C�  C�  C��3C�  C�  C�  C�  C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D �fDfD� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D�fD  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D!��D"� D#  D#y�D#��D$y�D%  D%y�D%��D&� D'fD'� D(  D(� D)  D)� D*  D*�fD+  D+� D,  D,� D-  D-�fD.  D.� D.��D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9�fD:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@fD@� D@��DA� DB  DB� DC  DC� DDfDD� DE  DE� DF  DF� DG  DG� DH  DH� DH��DIy�DJ  DJ� DK  DK� DL  DL� DM  DM� DM��DN� DO  DO� DO��DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DT��DU� DV  DV� DW  DW� DX  DX� DX��DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dc��Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dt� Du  Du� Dv  Dv� Dw  Dw� Dx  Dx� Dy  Dy� Dz  Dz� D{  D{� D|  D|� D}  D}� D~  D~� D  D� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�<�D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�C3D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�<�D�� D�� D�  D�@ D�� D�� D�  D�@ D��3D��3D�  D�@ D�� D�� D�  D�<�D�|�D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�<�D�|�D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D���D�  D�@ D�� D��3D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D���D���D�<�D�� D�� D�  D�@ D�� D�� D���D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D���D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D��3D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D��3D�3D�@ D��3D��3D�  D�@ D�� D���D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�C3D D�� D�  D�@ DÀ Dü�D�  D�@ DĀ D�� D�  D�@ Dŀ D�� D�  D�@ Dƀ D�� D�  D�@ Dǀ D�� D�  D�@ DȀ D�� D�  D�@ Dɀ D�� D�  D�@ Dʀ D�� D�  D�@ D˃3D�� D�  D�@ D̀ D̼�D�  D�@ D̀ D�� D�  D�@ D΀ D�� D�  D�@ D�|�D�� D�  D�@ DЀ D�� D�  D�@ Dр D�� D�  D�@ DҀ D�� D�  D�@ DӀ D�� D�  D�@ DԀ D�� D�  D�@ DՀ D�� D�  D�@ Dր D�� D�  D�@ D׀ D�� D�  D�@ D؀ D�� D�  D�@ D�|�Dټ�D�  D�@ Dڀ D�� D�  D�@ Dۀ D�� D�  D�@ D܀ D�� D�  D�@ D݀ D��3D�  D�@ Dހ D�� D�  D�@ D߀ D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�C3D� D�� D�  D�@ D�|�D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D��D���D�<�D�|�D�� D�  D�<�D� D�� D�  D�<�D�|�D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�p 1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   @���@ʏ\A�A%G�AEG�AeG�A���A���A���A��
A��
Aң�A��A��BQ�B	Q�BQ�BQ�B!Q�B)Q�B1Q�B9Q�BAQ�BH�BQQ�BYQ�BaQ�BiQ�BqQ�Bx�B���B��)B���B��)B��)B���B���B���B���B���B���B���B��)B��)B���B���B���BĨ�BȨ�B̨�BШ�BԨ�Bب�Bܨ�B��B��B��)B��B��B���B���B���C T{CT{CT{CT{CT{C
T{CT{CT{C:�CT{CT{CT{CT{CT{CT{CT{C T{C"T{C$T{C&T{C(:�C*T{C,:�C.T{C0T{C2T{C4T{C6T{C8T{C:T{C<T{C>T{C@T{CBT{CDT{CFT{CHT{CJT{CLnCNT{CPT{CRT{CTT{CVT{CXT{CZT{C\T{C^T{C`T{CbT{CdT{CfT{ChT{CjT{ClT{CnT{CpT{CrT{CtT{CvT{CxT{CzT{C|T{C~T{C�*=C�*=C�*=C�*=C�*=C�*=C�*=C�*=C�*=C�*=C�*=C�*=C�*=C�*=C�*=C�*=C�*=C�pC�pC�*=C�pC�pC�pC�*=C�*=C�*=C�7
C�*=C�*=C�*=C�*=C�*=C�*=C�*=C�*=C�*=C�*=C�*=C�*=C�*=C�*=C�*=C�7
C�*=C�*=C�*=C�*=C�*=C�*=C�*=C�*=C�*=C�*=C�*=C�*=C�*=C�pC�*=C�7
C�*=C�*=C�*=C�*=C�*=C�*=C�pC�*=C�*=C�*=C�*=C�pC�*=C�*=C�*=C�*=C�*=C�*=C�*=C�*=C�*=C�*=C�*=C�*=C�pC�*=C�*=C�*=C�*=C�*=C�*=C�*=C�*=C�*=C�*=C�*=C�*=C�*=C�*=C�*=C�*=C�*=C�*=C�*=C�*=C�*=C�*=C�*=C�*=C�*=C�*=C�*=C�*=C�*=C�*=C�7
C�7
C�*=C�*=C�*=C�*=C�*=C�*=C�*=C�*=C�*=C�*=C�*=C�*=D D ��D�D�DD�DD�DD�DD�DD�DD�DD�D	D	�D
D
�DD�DD�DD�DD�DD�DD�DD�DD�DD�DD�DD��DD�DD�DD�DD�DD�DD�DD�DD�DD�DD�D D �D!D!�D"�D"�D#D#��D$�D$��D%D%��D&�D&�D'�D'�D(D(�D)D)�D*D*��D+D+�D,D,�D-D-��D.D.�D/�D/�D0D0�D1D1�D2D2�D3D3�D4D4�D5D5�D6D6�D7D7�D8D8�D9D9��D:D:�D;D;�D<D<�D=D=�D>D>�D?D?�D@�D@�DA�DA�DBDB�DCDC�DD�DD�DEDE�DFDF�DGDG�DHDH�DI�DI��DJDJ�DKDK�DLDL�DMDM�DN�DN�DODO�DP�DP�DQDQ�DRDR�DSDS�DTDT�DU�DU�DVDV�DWDW�DXDX�DY�DY�DZDZ�D[D[�D\D\�D]D]�D^D^�D_D_�D`D`�DaDa�DbDb�DcDc�Dd�Dd�DeDe�DfDf�DgDg�DhDh�DiDi�DjDj�DkDk�DlDl�DmDm�DnDn�DoDo�DpDp�DqDq�DrDr�DsDs�DtDt�DuDu�DvDv�DwDw�DxDx�DyDy�DzDz�D{D{�D|D|�D}D}�D~D~�DD�D�
�D�J�D���D�ʏD�
�D�J�D���D�ʏD�
�D�G\D���D�ʏD�
�D�J�D���D�ʏD�
�D�J�D���D�ʏD�
�D�M�D���D�ʏD�
�D�J�D���D�ʏD�
�D�J�D���D�ʏD�
�D�J�D���D�ʏD�
�D�G\D���D�ʏD�
�D�J�D���D�ʏD�
�D�J�D���D���D�
�D�J�D���D�ʏD�
�D�G\D��\D�ʏD�
�D�J�D���D�ʏD�
�D�J�D���D�ʏD�
�D�J�D���D�ʏD�
�D�G\D��\D�ʏD�
�D�J�D���D�ʏD�
�D�J�D���D�ʏD�
�D�J�D���D��\D�
�D�J�D���D���D�
�D�J�D���D�ʏD�
�D�J�D���D�ʏD�
�D�J�D���D�ʏD�
�D�J�D���D�ʏD�
�D�J�D���D�ʏD�
�D�J�D���D�ʏD�
�D�J�D���D�ʏD�
�D�J�D���D�ʏD�
�D�J�D���D�ʏD�
�D�J�D���D��\D�\D�G\D���D�ʏD�
�D�J�D���D�ʏD�\D�J�D���D�ʏD�
�D�J�D���D�ʏD�
�D�J�D���D�ʏD�
�D�J�D���D�ʏD�
�D�J�D���D�ʏD�
�D�J�D���D�ʏD�
�D�J�D���D�ʏD�
�D�J�D���D�ʏD�
�D�J�D���D�ʏD�
�D�J�D���D�ʏD�
�D�J�D���D�ʏD�
�D�J�D���D�ʏD�
�D�J�D���D�ʏD�
�D�J�D���D�ʏD�
�D�J�D���D�ʏD�
�D�J�D���D�ʏD�
�D�J�D���D��\D�
�D�J�D���D�ʏD�
�D�J�D���D�ʏD�
�D�J�D���D�ʏD�
�D�J�D���D���D�
�D�J�D���D�ʏD�
�D�J�D���D�ʏD�
�D�J�D���D�ʏD�
�D�J�D���D���D��D�J�D���D���D�
�D�J�D���D��\D�
�D�J�D���D�ʏD�
�D�J�D���D�ʏD�
�D�J�D���D�ʏD�
�D�J�D���D�ʏD�
�D�J�D���D�ʏD�
�D�M�D�D�ʏD�
�D�J�DÊ�D��\D�
�D�J�DĊ�D�ʏD�
�D�J�DŊ�D�ʏD�
�D�J�DƊ�D�ʏD�
�D�J�DǊ�D�ʏD�
�D�J�DȊ�D�ʏD�
�D�J�DɊ�D�ʏD�
�D�J�Dʊ�D�ʏD�
�D�J�Dˍ�D�ʏD�
�D�J�D̊�D��\D�
�D�J�D͊�D�ʏD�
�D�J�DΊ�D�ʏD�
�D�J�Dχ\D�ʏD�
�D�J�DЊ�D�ʏD�
�D�J�Dъ�D�ʏD�
�D�J�DҊ�D�ʏD�
�D�J�Dӊ�D�ʏD�
�D�J�DԊ�D�ʏD�
�D�J�DՊ�D�ʏD�
�D�J�D֊�D�ʏD�
�D�J�D׊�D�ʏD�
�D�J�D؊�D�ʏD�
�D�J�Dه\D��\D�
�D�J�Dڊ�D�ʏD�
�D�J�Dۊ�D�ʏD�
�D�J�D܊�D�ʏD�
�D�J�D݊�D���D�
�D�J�Dފ�D�ʏD�
�D�J�Dߊ�D�ʏD�
�D�J�D���D�ʏD�
�D�J�D኏D�ʏD�
�D�J�D⊏D�ʏD�
�D�J�D㊏D�ʏD�
�D�J�D䊏D�ʏD�
�D�J�D劏D�ʏD�
�D�M�D抏D�ʏD�
�D�J�D�\D�ʏD�
�D�J�D芏D�ʏD�
�D�J�D銏D�ʏD�
�D�J�DꊏD�ʏD�
�D�J�D늏D�ʏD�
�D�J�D슏D�ʏD�
�D�J�D튏D�ʏD�
�D�J�DD�ʏD�
�D�J�DD�ʏD�
�D�J�D���D��\D�\D�G\D�\D�ʏD�
�D�G\D�D�ʏD�
�D�G\D�\D�ʏD�
�D�J�D�D�ʏD�
�D�J�D���D�ʏD�
�D�J�D���D�ʏD�
�D�J�D���D�ʏD�
�D�J�D���D�ʏD�
�D�J�D�z�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   A��A��A�  A�bA�VA�;dA�G�A�33A�;dA�M�A�bNA�n�A�|�A�~�A�x�A�dZAϙ�AϾwA�ĜA�ĜA�ĜA�ĜA�ȴA�ȴA���A���A���A��#A��#A���A���A�AϼjAϸRAϺ^AϮAϥ�Aϕ�AύPA�\)A��;A���A�  A��Aɲ-Aǣ�A�O�Aş�AĮA�z�A�=qA�
=A���A�z�A�oA�1A�E�A�1'A���A�^5A��7A�+A�5?A�K�A�v�A�1A�-A�E�A���A��A��A�(�A��A�;dA�z�A�x�A���A�/A��A��HA��jA�JA�O�A�"�A��!A��A�l�A�C�A�`BA�G�A�{A���A���A��TA�9XA�C�A��jA��PA�I�A���A��uA�M�A�ĜA�9XA�A�K�A�l�A��`A�  A���A}�
A|1A{�#Az��Aw�At�!Ap��Ap5?AoAn�!Alr�Aj$�Ai��Ah�AfAe`BAe33Ad��AdZAbr�A_��A[��AYp�AX�jAX1'AV��AS��ARA�AQ��AP��AOt�AL�AKoAJ�+AJJAI7LAG�-AE&�AD^5AC\)AAA?��A>jA=G�A<=qA;��A;O�A9��A8�/A7��A4��A2n�A0�/A0{A/x�A.�/A.VA-�A-��A,��A*�`A)��A)oA'��A'/A%A$1'A"1A!G�A ~�A��A&�A�+A-A��A7LAE�A�FAS�A��A�RA��AVA"�A�AI�A��AVA�A33AbNA�AG�A��A�AO�A�A��A��A5?AC�A
�HA
r�A
bA	��A	C�A�RA�At�AG�A�HA$�A�#AA�#A%A b@��@�`B@���@� �@��\@�X@�+@��@���@�P@��;@�M�@�/@띲@�\)@�R@���@�j@�\@��@��@��/@�ƨ@�-@�/@�@�z�@�K�@ާ�@ݙ�@�9X@�
=@�=q@ف@ׅ@ָR@��T@ԓu@�A�@�|�@�33@�n�@�/@��;@��y@�~�@�@��@��;@ˮ@�K�@ʗ�@��#@�%@Ȭ@�1@ǅ@�M�@Ł@�7L@��@Ĵ9@�1'@�1@�ƨ@�t�@�\)@��@\@�&�@��@�dZ@�C�@�C�@�;d@�ff@�bN@���@��m@�K�@���@���@�Q�@��@���@�|�@�n�@�J@���@�`B@�?}@��`@��9@�z�@�I�@��;@���@��@��!@��@���@�`B@��@���@�  @��P@�l�@��y@��\@��+@�ff@�$�@��T@���@�`B@�/@��`@��u@�I�@��
@���@�t�@�C�@��@��+@��@��#@��@���@���@��@��@�S�@�n�@�V@�/@��-@��-@�p�@�/@��`@��9@�r�@���@��j@�$�@���@��@�j@�  @��@��@�1'@�(�@��w@��w@�  @�I�@�Z@�I�@�9X@��@��F@�;d@���@��@��@��H@���@��\@�~�@�ff@�=q@��T@��^@�p�@�O�@�G�@�7L@�/@�&�@��`@���@��`@�Ĝ@���@�j@��F@�v�@�@��^@��`@��@��+@���@��9@��j@�1@�|�@�33@�
=@�ȴ@�^5@�{@���@�`B@��@��@��@�V@�/@�hs@��-@���@��#@�@�5?@�M�@�^5@�ff@��\@���@�~�@�M�@�-@�{@��@���@���@��7@�&�@���@��@��u@��@�z�@�Q�@�b@���@���@�dZ@�o@��@���@���@�=q@��@�@��-@�p�@�O�@�/@���@��@���@��D@�A�@���@�ƨ@�C�@��@���@�n�@�V@��@��T@��h@�hs@�/@��@���@�Q�@���@��@��@�C�@�+@��y@���@�n�@�=q@�-@�$�@�J@���@�/@�&�@�&�@�V@���@�Ĝ@���@��@�Z@�Z@�Q�@�I�@��@��
@��@�dZ@�\)@�C�@�@��+@��@��@��-@��@��@��@�P@\)@\)@~��@}�T@|�/@|z�@|1@{�@{@z�H@z��@z�!@z��@z=q@y��@xĜ@x��@x�@xA�@w�@wl�@w
=@vv�@v5?@u�@u�T@u��@u/@uV@t�@s��@s�@r�@r��@rn�@q�^@q�7@q�@p��@pr�@o�;@oK�@n��@nV@m�-@m�@l�@lj@l(�@k�m@kC�@j��@j-@i&�@h��@hQ�@h �@g��@g�P@g+@f�R@fE�@e@d��@d�/@d�j@dj@d1@c�m@c�
@c��@cdZ@c@bn�@b=q@a�@ahs@`��@`Q�@_|�@^�y@^�+@^5?@]�T@]��@]`B@]�@\�/@\(�@[��@[33@Z�@Z�H@Z��@Z��@Z^5@Y7L@X�9@X�@X1'@X  @W�@W�;@W�;@W�;@W|�@W�@V��@V��@Vv�@Vff@U�@U�h@U?}@U?}@U/@T�/@T�j@T�D@T9X@T1@S�m@St�@S@R�@R��@R��@R�\@Rn�@R^5@RJ@Q�7@Q�@P��@O�@O�P@O+@N�+@N5?@N5?@N{@N@M�T@Mp�@M�@L�D@L9X@K�
@KdZ@K33@Ko@J�@J��@J��@J~�@J=q@Ihs@I&�@I�@I�@I%@I%@I%@H�`@Hb@G�w@GK�@F��@F�@F�R@F��@F�+@FE�@E�@E�h@D�/@D�j@D(�@C�m@Ct�@C@B��@B~�@B�@A��@Ahs@@��@@�@@Q�@@b@@  @?�@?�@?l�@>�R@>@=�-@=��@=p�@<��@<j@<I�@;��@;��@;t�@;dZ@;33@:�!@:n�@:=q@9�^@9x�@9G�@9&�@8Ĝ@8��@8Q�@8 �@8 �@8  @7�@7��@7�P@7�P@7|�@7+@6��@6�@6ȴ@6��@6�+@6V@65?@5@5�@5O�@5?}@5/@5�@4�/@4�D@4�D@4��@4��@4�D@4j@4Z@49X@4�@3ƨ@3��@3C�@2�@2��@2~�@2^5@2-@1�@1��@1��@1��@1�^@1�^@1�7@1&�@1%@0Ĝ@0�u@0�@0r�@0Q�@0Q�@01'@0b@0  @/�P@/+@/
=@.�y@.ȴ@.�R@.��@.�+@.v�@.ff@.5?@.$�@.{@.@-��@-�-@-p�@-�@,��@,�/@,��@,Z@,9X@,9X@+��@+�
@+ƨ@+ƨ@+�F@+S�@*�!@*=q@*J@)��@)G�@)�@(Ĝ@(�u@(r�@(bN@(Q�@'�;@'�@'�P@'\)@'K�@'+@&�y@&v�@&@%��@%�@%`B@%/@$�@$�@$�@$�@$�@$��@$Z@#ƨ@#S�@#@"�@"��@"�!@"�!@"�\@"^5@"-@"�@"J@!��@!��@!hs@!7L@ ��@  �@   @��@��@��@��@��@\)@�@ȴ@5?@�T@p�@�@�/@�D@�D@(�@��@C�@C�@@��@~�@�#@��@�7@X@&�@�@�`@Ĝ@��@�@bN@�@��@+@
=@�@v�@{@�T@�-@`B@/@V@�@��@��@z�@Z@�@��@�F@��@��@�@"�@��@~�@n�@n�@^5@=q@-@�@�@-@��@X@��@Ĝ@�9@�9@�9@�9@�9@��@��@�@Q�@1'@ �@��@\)@
=@ȴ@�+@V@�@�h@�@�@p�@p�@p�@p�@p�@p�@p�@p�@�@��@�j1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   A��A��A�  A�bA�VA�;dA�G�A�33A�;dA�M�A�bNA�n�A�|�A�~�A�x�A�dZAϙ�AϾwA�ĜA�ĜA�ĜA�ĜA�ȴA�ȴA���A���A���A��#A��#A���A���A�AϼjAϸRAϺ^AϮAϥ�Aϕ�AύPA�\)A��;A���A�  A��Aɲ-Aǣ�A�O�Aş�AĮA�z�A�=qA�
=A���A�z�A�oA�1A�E�A�1'A���A�^5A��7A�+A�5?A�K�A�v�A�1A�-A�E�A���A��A��A�(�A��A�;dA�z�A�x�A���A�/A��A��HA��jA�JA�O�A�"�A��!A��A�l�A�C�A�`BA�G�A�{A���A���A��TA�9XA�C�A��jA��PA�I�A���A��uA�M�A�ĜA�9XA�A�K�A�l�A��`A�  A���A}�
A|1A{�#Az��Aw�At�!Ap��Ap5?AoAn�!Alr�Aj$�Ai��Ah�AfAe`BAe33Ad��AdZAbr�A_��A[��AYp�AX�jAX1'AV��AS��ARA�AQ��AP��AOt�AL�AKoAJ�+AJJAI7LAG�-AE&�AD^5AC\)AAA?��A>jA=G�A<=qA;��A;O�A9��A8�/A7��A4��A2n�A0�/A0{A/x�A.�/A.VA-�A-��A,��A*�`A)��A)oA'��A'/A%A$1'A"1A!G�A ~�A��A&�A�+A-A��A7LAE�A�FAS�A��A�RA��AVA"�A�AI�A��AVA�A33AbNA�AG�A��A�AO�A�A��A��A5?AC�A
�HA
r�A
bA	��A	C�A�RA�At�AG�A�HA$�A�#AA�#A%A b@��@�`B@���@� �@��\@�X@�+@��@���@�P@��;@�M�@�/@띲@�\)@�R@���@�j@�\@��@��@��/@�ƨ@�-@�/@�@�z�@�K�@ާ�@ݙ�@�9X@�
=@�=q@ف@ׅ@ָR@��T@ԓu@�A�@�|�@�33@�n�@�/@��;@��y@�~�@�@��@��;@ˮ@�K�@ʗ�@��#@�%@Ȭ@�1@ǅ@�M�@Ł@�7L@��@Ĵ9@�1'@�1@�ƨ@�t�@�\)@��@\@�&�@��@�dZ@�C�@�C�@�;d@�ff@�bN@���@��m@�K�@���@���@�Q�@��@���@�|�@�n�@�J@���@�`B@�?}@��`@��9@�z�@�I�@��;@���@��@��!@��@���@�`B@��@���@�  @��P@�l�@��y@��\@��+@�ff@�$�@��T@���@�`B@�/@��`@��u@�I�@��
@���@�t�@�C�@��@��+@��@��#@��@���@���@��@��@�S�@�n�@�V@�/@��-@��-@�p�@�/@��`@��9@�r�@���@��j@�$�@���@��@�j@�  @��@��@�1'@�(�@��w@��w@�  @�I�@�Z@�I�@�9X@��@��F@�;d@���@��@��@��H@���@��\@�~�@�ff@�=q@��T@��^@�p�@�O�@�G�@�7L@�/@�&�@��`@���@��`@�Ĝ@���@�j@��F@�v�@�@��^@��`@��@��+@���@��9@��j@�1@�|�@�33@�
=@�ȴ@�^5@�{@���@�`B@��@��@��@�V@�/@�hs@��-@���@��#@�@�5?@�M�@�^5@�ff@��\@���@�~�@�M�@�-@�{@��@���@���@��7@�&�@���@��@��u@��@�z�@�Q�@�b@���@���@�dZ@�o@��@���@���@�=q@��@�@��-@�p�@�O�@�/@���@��@���@��D@�A�@���@�ƨ@�C�@��@���@�n�@�V@��@��T@��h@�hs@�/@��@���@�Q�@���@��@��@�C�@�+@��y@���@�n�@�=q@�-@�$�@�J@���@�/@�&�@�&�@�V@���@�Ĝ@���@��@�Z@�Z@�Q�@�I�@��@��
@��@�dZ@�\)@�C�@�@��+@��@��@��-@��@��@��@�P@\)@\)@~��@}�T@|�/@|z�@|1@{�@{@z�H@z��@z�!@z��@z=q@y��@xĜ@x��@x�@xA�@w�@wl�@w
=@vv�@v5?@u�@u�T@u��@u/@uV@t�@s��@s�@r�@r��@rn�@q�^@q�7@q�@p��@pr�@o�;@oK�@n��@nV@m�-@m�@l�@lj@l(�@k�m@kC�@j��@j-@i&�@h��@hQ�@h �@g��@g�P@g+@f�R@fE�@e@d��@d�/@d�j@dj@d1@c�m@c�
@c��@cdZ@c@bn�@b=q@a�@ahs@`��@`Q�@_|�@^�y@^�+@^5?@]�T@]��@]`B@]�@\�/@\(�@[��@[33@Z�@Z�H@Z��@Z��@Z^5@Y7L@X�9@X�@X1'@X  @W�@W�;@W�;@W�;@W|�@W�@V��@V��@Vv�@Vff@U�@U�h@U?}@U?}@U/@T�/@T�j@T�D@T9X@T1@S�m@St�@S@R�@R��@R��@R�\@Rn�@R^5@RJ@Q�7@Q�@P��@O�@O�P@O+@N�+@N5?@N5?@N{@N@M�T@Mp�@M�@L�D@L9X@K�
@KdZ@K33@Ko@J�@J��@J��@J~�@J=q@Ihs@I&�@I�@I�@I%@I%@I%@H�`@Hb@G�w@GK�@F��@F�@F�R@F��@F�+@FE�@E�@E�h@D�/@D�j@D(�@C�m@Ct�@C@B��@B~�@B�@A��@Ahs@@��@@�@@Q�@@b@@  @?�@?�@?l�@>�R@>@=�-@=��@=p�@<��@<j@<I�@;��@;��@;t�@;dZ@;33@:�!@:n�@:=q@9�^@9x�@9G�@9&�@8Ĝ@8��@8Q�@8 �@8 �@8  @7�@7��@7�P@7�P@7|�@7+@6��@6�@6ȴ@6��@6�+@6V@65?@5@5�@5O�@5?}@5/@5�@4�/@4�D@4�D@4��@4��@4�D@4j@4Z@49X@4�@3ƨ@3��@3C�@2�@2��@2~�@2^5@2-@1�@1��@1��@1��@1�^@1�^@1�7@1&�@1%@0Ĝ@0�u@0�@0r�@0Q�@0Q�@01'@0b@0  @/�P@/+@/
=@.�y@.ȴ@.�R@.��@.�+@.v�@.ff@.5?@.$�@.{@.@-��@-�-@-p�@-�@,��@,�/@,��@,Z@,9X@,9X@+��@+�
@+ƨ@+ƨ@+�F@+S�@*�!@*=q@*J@)��@)G�@)�@(Ĝ@(�u@(r�@(bN@(Q�@'�;@'�@'�P@'\)@'K�@'+@&�y@&v�@&@%��@%�@%`B@%/@$�@$�@$�@$�@$�@$��@$Z@#ƨ@#S�@#@"�@"��@"�!@"�!@"�\@"^5@"-@"�@"J@!��@!��@!hs@!7L@ ��@  �@   @��@��@��@��@��@\)@�@ȴ@5?@�T@p�@�@�/@�D@�D@(�@��@C�@C�@@��@~�@�#@��@�7@X@&�@�@�`@Ĝ@��@�@bN@�@��@+@
=@�@v�@{@�T@�-@`B@/@V@�@��@��@z�@Z@�@��@�F@��@��@�@"�@��@~�@n�@n�@^5@=q@-@�@�@-@��@X@��@Ĝ@�9@�9@�9@�9@�9@��@��@�@Q�@1'@ �@��@\)@
=@ȴ@�+@V@�@�h@�@�@p�@p�@p�@p�@p�@p�@p�@p�@�@��@�j1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   B  BBBBB	7BPB+B
=BPBoB{B�B�B�B�B)�BA�BL�BM�BN�BO�BP�BQ�BW
BYBYB[#B[#BZBYBXBW
BW
BYB[#B]/BbNBhsBx�B��B��B��B"�B5?B;dB;dB;dB<jB;dB5?B/B9XB;dB(�B#�BBBB��BBB	7BhB�B0!B7LB=qB=qBC�BI�BK�BI�BI�BI�BC�B1'B,B,B�BbB%B��B�BÖB�'B��B��B�oB�Bx�Bp�Bm�BhsBN�B?}B8RB33B0!B$�B\B
��B
�B
�fB
�B
��B
�3B
��B
�\B
}�B
]/B
L�B
I�B
A�B
/B
�B

=B
  B	��B	��B	�mB	�)B	�
B	��B	��B	�dB	�^B	�LB	�3B	��B	��B	� B	r�B	m�B	jB	bNB	ZB	N�B	M�B	G�B	C�B	6FB	/B	+B	(�B	$�B	�B	uB	VB	
=B	+B	B	  B��B��B��B�B�B�yB�mB�`B�#B�
B��B��B��B��B��B��BɺBĜB��B�qB�^B�?B�3B�!B�B��B��B��B��B�B�B�B�B�B�B�B�B��B��B��B�B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B�B�B�B�B�B�B�B��B��B��B��B��B��B�!B�?B�FB�FB�?B�LB�RB�XB�wB�jB�qB�}B�wB��B��BĜBǮBȴBȴBȴB��B��B��B��B��B��B��B��B��B�B�
B�
B�
B�B�#B�BB�BB�TB�ZB�sB�B�B�B�B�B�B��B��B��B��B��B	  B	B	B	B	
=B	PB	VB	bB	hB	oB	uB	{B	�B	�B	�B	�B	�B	!�B	$�B	%�B	&�B	'�B	(�B	'�B	'�B	-B	0!B	2-B	2-B	5?B	9XB	D�B	L�B	P�B	R�B	T�B	ZB	aHB	dZB	e`B	gmB	iyB	iyB	iyB	iyB	m�B	p�B	s�B	t�B	v�B	x�B	{�B	}�B	� B	�B	�B	�B	�+B	�7B	�=B	�=B	�PB	�bB	�bB	�oB	�oB	�{B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�hB	�\B	�\B	�bB	�uB	��B	��B	��B	��B	��B	��B	��B	��B	��B	�-B	�3B	�'B	�3B	�3B	�FB	�XB	�jB	�}B	�}B	��B	ĜB	ɺB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�
B	�
B	�
B	�B	�)B	�;B	�`B	�mB	�sB	�yB	�B	�B	�yB	�sB	�sB	�`B	�TB	�NB	�HB	�TB	�`B	�fB	�mB	�sB	�yB	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B
  B
B
B
B
B
%B
+B
+B
+B
+B
1B
1B
	7B

=B
DB
JB
JB
DB
DB
DB
JB
JB
DB
JB
PB
VB
VB
\B
\B
bB
bB
hB
hB
hB
oB
oB
uB
uB
uB
uB
{B
�B
�B
�B
�B
�B
�B
�B
�B
�B
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
"�B
"�B
#�B
#�B
#�B
#�B
#�B
#�B
#�B
$�B
$�B
%�B
%�B
%�B
&�B
&�B
&�B
&�B
&�B
%�B
$�B
#�B
$�B
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
)�B
)�B
)�B
+B
-B
,B
,B
,B
-B
-B
-B
.B
.B
/B
/B
/B
/B
0!B
0!B
0!B
1'B
1'B
2-B
1'B
2-B
33B
2-B
33B
33B
49B
49B
49B
5?B
5?B
6FB
7LB
7LB
7LB
7LB
7LB
8RB
8RB
8RB
9XB
9XB
9XB
:^B
:^B
:^B
:^B
:^B
;dB
;dB
<jB
=qB
>wB
>wB
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
C�B
C�B
D�B
E�B
E�B
F�B
F�B
F�B
F�B
F�B
F�B
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
P�B
P�B
Q�B
Q�B
Q�B
Q�B
Q�B
Q�B
Q�B
Q�B
Q�B
R�B
R�B
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
XB
XB
XB
XB
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
`BB
`BB
`BB
_;B
_;B
_;B
_;B
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
bNB
aHB
bNB
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
cTB
dZB
dZB
dZB
e`B
ffB
ffB
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
jB
jB
jB
jB
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
l�B
l�B
m�B
m�B
m�B
m�B
m�B
m�B
n�B
n�B
n�B
n�B
n�B
o�B
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
r�B
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
t�B
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
w�B
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
y�B
z�B
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
|�B
|�B
|�B
|�B
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
�B
�B
�B
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
�B
�B
�B
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
�B
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
�+B
�+B
�+B
�+B
�+B
�+B
�+B
�+B
�+B
�+B
�+B
�+B
�+B
�+1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   B  BBBBB	7BPB+B
=BPBoB{B�B�B�B�B)�BA�BL�BM�BN�BO�BP�BQ�BW
BYBYB[#B[#BZBYBXBW
BW
BYB[#B]/BbNBhsBx�B��B��B��B"�B5?B;dB;dB;dB<jB;dB5?B/B9XB;dB(�B#�BBBB��BBB	7BhB�B0!B7LB=qB=qBC�BI�BK�BI�BI�BI�BC�B1'B,B,B�BbB%B��B�BÖB�'B��B��B�oB�Bx�Bp�Bm�BhsBN�B?}B8RB33B0!B$�B\B
��B
�B
�fB
�B
��B
�3B
��B
�\B
}�B
]/B
L�B
I�B
A�B
/B
�B

=B
  B	��B	��B	�mB	�)B	�
B	��B	��B	�dB	�^B	�LB	�3B	��B	��B	� B	r�B	m�B	jB	bNB	ZB	N�B	M�B	G�B	C�B	6FB	/B	+B	(�B	$�B	�B	uB	VB	
=B	+B	B	  B��B��B��B�B�B�yB�mB�`B�#B�
B��B��B��B��B��B��BɺBĜB��B�qB�^B�?B�3B�!B�B��B��B��B��B�B�B�B�B�B�B�B�B��B��B��B�B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B�B�B�B�B�B�B�B��B��B��B��B��B��B�!B�?B�FB�FB�?B�LB�RB�XB�wB�jB�qB�}B�wB��B��BĜBǮBȴBȴBȴB��B��B��B��B��B��B��B��B��B�B�
B�
B�
B�B�#B�BB�BB�TB�ZB�sB�B�B�B�B�B�B��B��B��B��B��B	  B	B	B	B	
=B	PB	VB	bB	hB	oB	uB	{B	�B	�B	�B	�B	�B	!�B	$�B	%�B	&�B	'�B	(�B	'�B	'�B	-B	0!B	2-B	2-B	5?B	9XB	D�B	L�B	P�B	R�B	T�B	ZB	aHB	dZB	e`B	gmB	iyB	iyB	iyB	iyB	m�B	p�B	s�B	t�B	v�B	x�B	{�B	}�B	� B	�B	�B	�B	�+B	�7B	�=B	�=B	�PB	�bB	�bB	�oB	�oB	�{B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�hB	�\B	�\B	�bB	�uB	��B	��B	��B	��B	��B	��B	��B	��B	��B	�-B	�3B	�'B	�3B	�3B	�FB	�XB	�jB	�}B	�}B	��B	ĜB	ɺB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�
B	�
B	�
B	�B	�)B	�;B	�`B	�mB	�sB	�yB	�B	�B	�yB	�sB	�sB	�`B	�TB	�NB	�HB	�TB	�`B	�fB	�mB	�sB	�yB	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B
  B
B
B
B
B
%B
+B
+B
+B
+B
1B
1B
	7B

=B
DB
JB
JB
DB
DB
DB
JB
JB
DB
JB
PB
VB
VB
\B
\B
bB
bB
hB
hB
hB
oB
oB
uB
uB
uB
uB
{B
�B
�B
�B
�B
�B
�B
�B
�B
�B
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
"�B
"�B
#�B
#�B
#�B
#�B
#�B
#�B
#�B
$�B
$�B
%�B
%�B
%�B
&�B
&�B
&�B
&�B
&�B
%�B
$�B
#�B
$�B
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
)�B
)�B
)�B
+B
-B
,B
,B
,B
-B
-B
-B
.B
.B
/B
/B
/B
/B
0!B
0!B
0!B
1'B
1'B
2-B
1'B
2-B
33B
2-B
33B
33B
49B
49B
49B
5?B
5?B
6FB
7LB
7LB
7LB
7LB
7LB
8RB
8RB
8RB
9XB
9XB
9XB
:^B
:^B
:^B
:^B
:^B
;dB
;dB
<jB
=qB
>wB
>wB
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
C�B
C�B
D�B
E�B
E�B
F�B
F�B
F�B
F�B
F�B
F�B
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
P�B
P�B
Q�B
Q�B
Q�B
Q�B
Q�B
Q�B
Q�B
Q�B
Q�B
R�B
R�B
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
XB
XB
XB
XB
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
`BB
`BB
`BB
_;B
_;B
_;B
_;B
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
bNB
aHB
bNB
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
cTB
dZB
dZB
dZB
e`B
ffB
ffB
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
jB
jB
jB
jB
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
l�B
l�B
m�B
m�B
m�B
m�B
m�B
m�B
n�B
n�B
n�B
n�B
n�B
o�B
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
r�B
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
t�B
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
w�B
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
y�B
z�B
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
|�B
|�B
|�B
|�B
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
�B
�B
�B
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
�B
�B
�B
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
�B
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
�+B
�+B
�+B
�+B
�+B
�+B
�+B
�+B
�+B
�+B
�+B
�+B
�+B
�+1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                            JA  ARFMdecpA19c                                                                20220105004107  IP                  G�O�G�O�G�O�                JA  ARFMfmtp2.8                                                                 20220104154344  IP                  G�O�G�O�G�O�                JA  ARCAsspa3.1b                                                                20220104154345  IP  PRES            G�O�G�O�G�O�                JA  ARGQrqcppo_c                                                                20220104154345  QCP$                G�O�G�O�G�O�              3CJA  ARGQrqcpt19d                                                                20220104154345  QCP$                G�O�G�O�G�O�           80000JA  ARGQaqcpt19d                                                                20220104154345  QCP$                G�O�G�O�G�O�           80000JA  ARGQrqcp2.8e                                                                20220104154345  QCP$                G�O�G�O�G�O�            FB40JA  ARGQaqcp2.8e                                                                20220104154345  QCP$                G�O�G�O�G�O�            FB40JA  ARGQrqcpt16c                                                                20220104154346  QCP$                G�O�G�O�G�O�           10000JA      jafc1.0                                                                 20220104154346                      G�O�G�O�G�O�                JA  ARUP                                                                        20220104155208                      G�O�G�O�G�O�                