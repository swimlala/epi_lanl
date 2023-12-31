CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       JMA    source        
Argo float     history       E2022-06-04T17:48:56Z creation;2022-06-04T17:48:56Z conversion to V3.1      
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
_FillValue                 �  I8   PRES_ADJUSTED            
      	   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  M(   PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  \�   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     
_FillValue        G�O�   	valid_min         �      	valid_max         B      C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  `�   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  p�   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     
_FillValue        G�O�   	valid_min         �      	valid_max         B      C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  tx   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �0   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    
_FillValue        G�O�   	valid_min         @      	valid_max         B$     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  �    PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ��   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    
_FillValue        G�O�   	valid_min         @      	valid_max         B$     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  ��   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ��   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         decibar    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  �p   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         degree_Celsius     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  �(   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  ��   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  0  ޘ   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                    ��   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                    ��   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                    ��   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  ,  ��   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                    ��   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                    ��   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                    ��   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                    �    HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  @  �   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                    �D   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                    �T   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    �X   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar         �h   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar         �l   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�        �p   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    �tArgo profile    3.1 1.2 19500101000000  20220604174856  20220610141505  5905854                                                                 JAMSTEC                                                         PRES            TEMP            PSAL               A   JA                                  2B  A   APEX                            8422                            2.11.2                          846 @��H���1   @��I[�S@.%`A�7L�cI$�/1   GPS     A   A   A   Primary sampling: averaged [2 dbar bin average for 1 Hz CTD]                                                                                                                                                                                                       @��@�  @�  A��AffA>ffA^ffA�  A�  A�  A�  A�  A�  A�  A�  B   B  B  B  B   B(  B0  B8  B@  BH  BP  BX  B`  Bh  Bp  Bx  B�33B���B���B�  B�  B�  B�ffB�ffB�  B�  B�  B���B���B�  B�  B�  B���B�  B�  B̙�B�ffB�  B�  B�  B�  B�  B�  B�  B�33B�33B���B�33C   C  C  C  C  C
  C  C  C  C  C  C  C�C�C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD�CF�CH  CJ  CL  CN  CP  CR  CT  CV  CW�fCY�fC\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C��C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg�fDh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dt� Du  Du� Dv  Dv� Dw  Dw� Dx  Dx� Dy  Dy� Dz  Dz� D{  D{� D|  D|� D}  D}� D~  D~� D  D� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D���D���D�<�D�|�D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D��3D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D��3D�� D�  D�@ D D�� D�  D�@ DÀ D�� D�  D�@ DĀ D�� D�  D�@ Dŀ D�� D�  D�@ Dƀ D�� D�  D�@ Dǀ D�� D�  D�@ DȀ D�� D�  D�@ Dɀ D�� D�  D�@ Dʀ D�� D�  D�@ Dˀ D�� D�  D�@ D̀ D�� D�  D�@ D̀ D�� D�  D�@ D΀ D�� D�  D�@ Dπ D�� D�  D�@ DЀ D�� D�  D�@ Dр D�� D�  D�@ DҀ D�� D�  D�@ DӀ D�� D�  D�@ DԀ D�� D�  D�@ DՀ D�� D�  D�@ Dր D�� D�  D�@ D׀ D�� D�  D�@ D؀ D�� D�  D�@ Dـ D�� D�  D�@ Dڀ D�� D�  D�@ Dۀ D�� D�  D�@ D܀ D�� D�  D�@ D݀ D�� D�  D�@ Dހ D�� D�  D�@ D߀ D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D���D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�vf1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  @   @�33@�33A34A   A@  A`  A���A���A���A���A���A���A���A���B ffBffBffBffB ffB(ffB0ffB8ffB@ffBHffBPffBXffB`ffBhffBpffBxffB�ffB�  B�  B�33B�33B�33B���B���B�33B�33B�33B�  B�  B�33B�33B�33B�  B�33B�33B���Bϙ�B�33B�33B�33B�33B�33B�33B�33B�ffB�ffB���B�ffC �C�C�C�C�C
�C�C�C�C�C�C�C34C34C�C�C �C"�C$�C&�C(�C*�C,�C.�C0�C2�C4�C6�C8�C:�C<�C>�C@�CB�CD34CF34CH�CJ�CL�CN�CP�CR�CT�CV�CX  CZ  C\�C^�C`�Cb�Cd�Cf�Ch�Cj�Cl�Cn�Cp�Cr�Ct�Cv�Cx�Cz�C|�C~�C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��D fD �fDfD�fDfD�fDfD�fDfD�fDfD�fDfD�fDfD�fDfD�fD	fD	�fD
fD
�fDfD�fDfD�fDfD�fDfD�fDfD�fDfD�fDfD�fDfD�fDfD�fDfD�fDfD�fDfD�fDfD�fDfD�fDfD�fDfD�fDfD�fDfD�fDfD�fDfD�fDfD�fD fD �fD!fD!�fD"fD"�fD#fD#�fD$fD$�fD%fD%�fD&fD&�fD'fD'�fD(fD(�fD)fD)�fD*fD*�fD+fD+�fD,fD,�fD-fD-�fD.fD.�fD/fD/�fD0fD0�fD1fD1�fD2fD2�fD3fD3�fD4fD4�fD5fD5�fD6fD6�fD7fD7�fD8fD8�fD9fD9�fD:fD:�fD;fD;�fD<fD<�fD=fD=�fD>fD>�fD?fD?�fD@fD@�fDAfDA�fDBfDB�fDCfDC�fDDfDD�fDEfDE�fDFfDF�fDGfDG�fDHfDH�fDIfDI�fDJfDJ�fDKfDK�fDLfDL�fDMfDM�fDNfDN�fDOfDO�fDPfDP�fDQfDQ�fDRfDR�fDSfDS�fDTfDT�fDUfDU�fDVfDV�fDWfDW�fDXfDX�fDYfDY�fDZfDZ�fD[fD[�fD\fD\�fD]fD]�fD^fD^�fD_fD_�fD`fD`�fDafDa�fDbfDb�fDcfDc�fDdfDd�fDefDe�fDffDf�fDgfDg��DhfDh�fDifDi�fDjfDj�fDkfDk�fDlfDl�fDmfDm�fDnfDn�fDofDo�fDpfDp�fDqfDq�fDrfDr�fDsfDs�fDtfDt�fDufDu�fDvfDv�fDwfDw�fDxfDx�fDyfDy�fDzfDz�fD{fD{�fD|fD|�fD}fD}�fD~fD~�fDfD�fD�3D�C3D��3D��3D�3D�C3D��3D��3D�3D�C3D��3D��3D�3D�C3D��3D��3D�3D�C3D��3D��3D�3D�C3D��3D��3D�3D�C3D��3D��3D�3D�C3D��3D��3D�3D�C3D��3D��3D�3D�C3D��3D��3D�3D�C3D��3D��3D�3D�C3D��3D��3D�3D�C3D��3D��3D�3D�C3D��3D��3D�3D�C3D��3D��3D�3D�C3D��3D��3D�3D�C3D��3D��3D�3D�C3D��3D�� D�  D�@ D�� D��3D�3D�C3D��3D��3D�3D�C3D��3D��3D�3D�C3D��3D��3D�3D�C3D��3D��3D�3D�C3D��3D��3D�3D�C3D��3D��3D�3D�C3D��3D��3D�3D�C3D��3D��3D�3D�C3D��3D��3D�3D�C3D��3D��3D�3D�C3D��3D��3D�3D�C3D��3D��3D�3D�C3D��3D��3D�3D�C3D��3D��3D�3D�C3D��3D��3D�3D�C3D��3D��3D�3D�C3D��3D��3D�3D�C3D��3D��3D�3D�C3D��3D��3D�3D�C3D��3D��3D�3D�C3D��3D��3D�3D�C3D��3D��3D�3D�C3D��3D��3D�3D�C3D��3D��3D�3D�C3D��3D��fD�3D�C3D��3D��3D�3D�C3D��3D��3D�3D�C3D��3D��3D�3D�C3D��3D��3D�3D�C3D��3D��3D�3D�C3D��3D��3D�3D�C3D��3D��3D�3D�C3D��3D��3D�3D�C3D��3D��3D�3D�C3D��3D��3D�3D�C3D��3D��3D�3D�C3D��3D��3D�3D�C3D��3D��3D�3D�C3D��3D��3D�3D�C3D��3D��3D�3D�C3D��3D��3D�3D�C3D��3D��3D�3D�C3D��3D��3D�3D�C3D��3D��3D�3D�C3D��3D��3D�3D�C3D��3D��3D�3D�C3D��fD��3D�3D�C3D3D��3D�3D�C3DÃ3D��3D�3D�C3Dă3D��3D�3D�C3DŃ3D��3D�3D�C3Dƃ3D��3D�3D�C3Dǃ3D��3D�3D�C3Dȃ3D��3D�3D�C3DɃ3D��3D�3D�C3Dʃ3D��3D�3D�C3D˃3D��3D�3D�C3D̃3D��3D�3D�C3D̓3D��3D�3D�C3D΃3D��3D�3D�C3Dσ3D��3D�3D�C3DЃ3D��3D�3D�C3Dу3D��3D�3D�C3D҃3D��3D�3D�C3DӃ3D��3D�3D�C3Dԃ3D��3D�3D�C3DՃ3D��3D�3D�C3Dփ3D��3D�3D�C3D׃3D��3D�3D�C3D؃3D��3D�3D�C3Dك3D��3D�3D�C3Dڃ3D��3D�3D�C3Dۃ3D��3D�3D�C3D܃3D��3D�3D�C3D݃3D��3D�3D�C3Dރ3D��3D�3D�C3D߃3D��3D�3D�C3D��3D��3D�3D�C3D�3D��3D�3D�C3D�3D��3D�3D�C3D�3D��3D�  D�C3D�3D��3D�3D�C3D�3D��3D�3D�C3D�3D��3D�3D�C3D�3D��3D�3D�C3D�3D��3D�3D�C3D�3D��3D�3D�C3D�3D��3D�3D�C3D�3D��3D�3D�C3D�3D��3D�3D�C3D�3D��3D�3D�C3D�3D��3D�3D�C3D�3D��3D�3D�C3D��3D��3D�3D�C3D�3D��3D�3D�C3D�3D��3D�3D�C3D�3D��3D�3D�C3D�3D��3D�3D�C3D��3D��3D�3D�C3D��3D��3D�3D�C3D��3D��3D�3D�C3D��3D��3D�3D�C3D��3D��3D�3D�C3D��3D��3D�3D�C3D�y�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  A�N�A�B�A�*�A␗A��A�W�A��2A�P�A߻0Aߣ�AߦAߟ�AߚAߟVAߤ�AߋxA߆YA߃�A�y�A�zxA�x�A�m�A�c�A�^5A�\�A�bA�f2A�`�A�[�A�\�A�6A�-CA��tA�O�A�ffAц�A�+kAͥ�A̯A�A�l"A��A�[#Aǫ�A�e,A���A���A�ÖA���A�'A�|PA��A�pA��bA���A�K�A�;dA��0A�ƨA�RTA��A�0!A��^A���A�L�A��nA��vA���A�)*A��TA�&�A��9A~��Ay��As�}Aq��Ane,Al�YAi($AgkQAc1�A_B�AZ��AW�AS��AQ\�ANɆALAL;dAJ�AI�bAG��AFYKAC��A@�mA>��A<��A:1'A8�DA6�hA5-�A3f�A2�A/�oA.D�A-�+A+͟A)��A(�XA&�A%��A%a�A%.IA%/�A%�fA%͟A%�A%��A$ĜA#��A"�A"SA"�4A"XA!�dA!MA (�A��A�A�PAl"A��A�bA��A?A1�A�QA�A�mA�kA�zA�oA��Ae,AĜAA��AOvAo�A�VA�}A7A�Ac�A��A#�An�A1�A�LAL�A-�A��A�uA_A�SAp;AdZA0UA��A��A�A��A\�A��A��AOvA�jAC�A��A
�pA
&A	�A	�eA	�A��Az�Ae�A!�A��A3�A�jA;dA��A�Ac A��AW?ATaA/�A	�A�,AMA i�A !@��h@��@��A@���@��@�-@��n@���@���@�'R@��@��6@���@���@���@�=�@���@�w�@��o@�X@�N<@���@�	�@�O�@���@�h�@�@�Vm@@�{J@�s�@��g@�a�@ꄶ@�G@�5�@�oi@�M@�@旍@���@�V@䩓@�ϫ@�@�0U@��@�F@��@�Ɇ@�M@ߗ$@�<6@��@�YK@�˒@�j@�%@ܦL@��@ږ�@�e@�_p@؍�@�x�@��/@֏\@ՠ�@��@�h
@�b@Ӝ�@�҉@Ҭ�@��@ѓ�@�O@�S@к�@�kQ@�!�@��r@ψf@��@�oi@�e@���@͑h@�e�@�F�@�9�@���@��@�c�@��@ʣ�@�Q@ɀ4@��@��8@ȓu@�W�@�~@��m@�c@���@��@Ɵ�@�^5@�#:@�{J@�I�@þw@Ë�@�Y�@�*0@A@�ݘ@���@�7L@�҉@�+k@���@��@��@��$@��o@��A@��g@�u�@��!@��T@���@�~�@�o@��'@�s�@�3�@�
�@��@�&�@���@�m�@��C@�X@��M@���@�u%@��@�K�@��]@�Z@�)�@��@���@���@��q@���@�c�@��@���@�q@�K^@�+k@�  @��3@���@�4�@�ߤ@���@�s�@�1�@��@��W@��H@�@�Ɇ@��$@���@�3�@��@��a@��@�t�@�T�@�,�@��@��<@��@�@�@��@�˒@��@�=@� i@��@��r@��A@��0@���@�`B@� i@��U@�.�@��@�˒@��w@�*0@���@�v�@��@��3@���@�/@��@�	@���@��M@�a@�#�@��@��@��)@�~�@�@���@�:�@��M@��1@�L0@�	�@���@�iD@�C@���@��h@�q@�A�@�6@���@�`B@�+@��H@���@�U2@��A@�J�@� \@��@��c@��?@�Z�@���@��=@�:�@��/@��9@�e�@��@�j@�q@�;@��f@��v@�҉@���@�j@�YK@�'R@�ϫ@��@��@��6@�n�@�@�@��t@�g�@�Y@���@�6�@���@�iD@��@���@�~(@�~@���@���@�@O@���@���@�l"@�9X@��@��'@�N<@�0�@�	l@��|@���@�y>@�V�@�<�@��]@�ԕ@���@�g�@�Mj@�G�@�,�@��,@���@�^5@�~@��K@���@�|�@�O�@��8@��L@�Q@�,=@��@�J@��g@���@��f@�s@�&@��?@��Y@�oi@�Xy@�C-@�(�@��@@��f@�f�@�/@��v@���@�u%@�j@�a|@�;�@��@��)@�@�f�@�q@���@��[@��@�˒@��f@�]�@�8@��@���@��x@���@���@�@�@�M@�@E9@o@~�@~�R@~�h@~�A@~J@}[W@}�@}@@|�[@|j@|!@{��@z��@z�!@z?@z �@yF@x��@x	�@w��@w'�@v{�@uzx@t�5@t�@s�r@s��@s�@r�@rJ�@q�@qzx@q	l@p�5@p��@p��@p�@o�r@o�@o�q@oA�@o�@n��@n@�@m�Z@m��@l��@l�D@lj@l4n@k�&@k�:@kRT@k.I@j�2@j}V@i�>@i[W@h�f@h��@g�}@g�@gv`@g�@fi�@f�@e�3@eN<@d�[@dU2@c�@cF�@b�s@b��@b6�@a��@a*0@`��@_�F@_A�@^�,@^h
@^E�@^!�@^�@]�@]��@]o @][W@]5�@\z�@\1@[�&@Z�1@Z_�@Z�@Y��@YF@Y/@Y!�@X��@X�@W�@W��@W��@W+@V��@V�x@V3�@U��@U��@U/@T��@T��@Tm�@TM@S�@S1�@R��@R4@Q�9@Q��@Q*0@P��@P~@Oy�@O1�@O�@N�"@N�\@M�)@M�@M�@LɆ@L[�@Kخ@KW?@KS@J��@J�R@J\�@J3�@Ju@I�@IL�@H��@H��@HbN@H4n@H1@Gj�@G(@F�R@Fu%@Eϫ@E+�@E�@D��@C�}@C��@C��@C,�@B��@A�D@A��@AVm@A%F@A@@@��@@z�@@S�@@:�@@@?��@?�q@?|�@?Mj@>��@>��@>�@>��@>Ov@=�@=�@=��@=Dg@=8�@=�@<�?@<C-@;�]@;�@;��@;�f@;�@:�@:a|@:�@9�>@9�^@9u�@9q@8�p@8[�@8!@7� @7j�@7C@6͟@6��@6GE@6�@5��@5�h@5T�@4�@4��@4$@3��@3�@3iD@2�8@2{�@2=q@1�d@1Dg@0��@0/�@/�@/��@/b�@/H�@/&@.��@.��@.}V@.:*@.�@-�o@-��@-:�@,�@,~(@,H@,�@+�+@+�W@+ƨ@+X�@+�@+o@+�@*��@*�,@*��@*��@*i�@* �@)��@)o @)!�@(�K@(��@(M@'�m@'��@'��@'�{@'{J@'v`@'U�@'Y@'S@&��@&��@&\�@&+k@%�D@%�-@%�@%L�@$��@$��@$~(@$Ft@#x@#Y@"�@"�R@"v�@"ff@"J�@"8�@"8�@"5?@"�@!�z@!�'@!p�@!Vm@!4@ �?@ �@ V�@ <�@ 6@ ,=@ !@ 7@ �@��@��@F�@�@�@{�@C�@	@�@��@2a@�@�E@�@j@[�@4n@@��@y�@@O@&@'�@Y@@�@�@��@v�@�.@}�@4@*0@	l@�	@�5@�E@��@~@�@˒@�F@]�@@O@8@4�@��@@�@	@��@�9@��@X@�@֡@��@z�@U2@H@�@��@�*@��@o�@P�@1�@�@�"@ȴ@v�@&�@#:@+k@1�@3�@O@��@�^@�t@�S@f�@�P@��@��@��@�z@��@w�@r�@m�@<�@��@��@�g@��@�w@�:@4�@�@�@��@�r@xl@~�@��@�r@�A@h
@M�@�@�3@�@��@e,@F@+�@�@�@��@9X@�F@\)@;d@)_@1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  A�N�A�B�A�*�A␗A��A�W�A��2A�P�A߻0Aߣ�AߦAߟ�AߚAߟVAߤ�AߋxA߆YA߃�A�y�A�zxA�x�A�m�A�c�A�^5A�\�A�bA�f2A�`�A�[�A�\�A�6A�-CA��tA�O�A�ffAц�A�+kAͥ�A̯A�A�l"A��A�[#Aǫ�A�e,A���A���A�ÖA���A�'A�|PA��A�pA��bA���A�K�A�;dA��0A�ƨA�RTA��A�0!A��^A���A�L�A��nA��vA���A�)*A��TA�&�A��9A~��Ay��As�}Aq��Ane,Al�YAi($AgkQAc1�A_B�AZ��AW�AS��AQ\�ANɆALAL;dAJ�AI�bAG��AFYKAC��A@�mA>��A<��A:1'A8�DA6�hA5-�A3f�A2�A/�oA.D�A-�+A+͟A)��A(�XA&�A%��A%a�A%.IA%/�A%�fA%͟A%�A%��A$ĜA#��A"�A"SA"�4A"XA!�dA!MA (�A��A�A�PAl"A��A�bA��A?A1�A�QA�A�mA�kA�zA�oA��Ae,AĜAA��AOvAo�A�VA�}A7A�Ac�A��A#�An�A1�A�LAL�A-�A��A�uA_A�SAp;AdZA0UA��A��A�A��A\�A��A��AOvA�jAC�A��A
�pA
&A	�A	�eA	�A��Az�Ae�A!�A��A3�A�jA;dA��A�Ac A��AW?ATaA/�A	�A�,AMA i�A !@��h@��@��A@���@��@�-@��n@���@���@�'R@��@��6@���@���@���@�=�@���@�w�@��o@�X@�N<@���@�	�@�O�@���@�h�@�@�Vm@@�{J@�s�@��g@�a�@ꄶ@�G@�5�@�oi@�M@�@旍@���@�V@䩓@�ϫ@�@�0U@��@�F@��@�Ɇ@�M@ߗ$@�<6@��@�YK@�˒@�j@�%@ܦL@��@ږ�@�e@�_p@؍�@�x�@��/@֏\@ՠ�@��@�h
@�b@Ӝ�@�҉@Ҭ�@��@ѓ�@�O@�S@к�@�kQ@�!�@��r@ψf@��@�oi@�e@���@͑h@�e�@�F�@�9�@���@��@�c�@��@ʣ�@�Q@ɀ4@��@��8@ȓu@�W�@�~@��m@�c@���@��@Ɵ�@�^5@�#:@�{J@�I�@þw@Ë�@�Y�@�*0@A@�ݘ@���@�7L@�҉@�+k@���@��@��@��$@��o@��A@��g@�u�@��!@��T@���@�~�@�o@��'@�s�@�3�@�
�@��@�&�@���@�m�@��C@�X@��M@���@�u%@��@�K�@��]@�Z@�)�@��@���@���@��q@���@�c�@��@���@�q@�K^@�+k@�  @��3@���@�4�@�ߤ@���@�s�@�1�@��@��W@��H@�@�Ɇ@��$@���@�3�@��@��a@��@�t�@�T�@�,�@��@��<@��@�@�@��@�˒@��@�=@� i@��@��r@��A@��0@���@�`B@� i@��U@�.�@��@�˒@��w@�*0@���@�v�@��@��3@���@�/@��@�	@���@��M@�a@�#�@��@��@��)@�~�@�@���@�:�@��M@��1@�L0@�	�@���@�iD@�C@���@��h@�q@�A�@�6@���@�`B@�+@��H@���@�U2@��A@�J�@� \@��@��c@��?@�Z�@���@��=@�:�@��/@��9@�e�@��@�j@�q@�;@��f@��v@�҉@���@�j@�YK@�'R@�ϫ@��@��@��6@�n�@�@�@��t@�g�@�Y@���@�6�@���@�iD@��@���@�~(@�~@���@���@�@O@���@���@�l"@�9X@��@��'@�N<@�0�@�	l@��|@���@�y>@�V�@�<�@��]@�ԕ@���@�g�@�Mj@�G�@�,�@��,@���@�^5@�~@��K@���@�|�@�O�@��8@��L@�Q@�,=@��@�J@��g@���@��f@�s@�&@��?@��Y@�oi@�Xy@�C-@�(�@��@@��f@�f�@�/@��v@���@�u%@�j@�a|@�;�@��@��)@�@�f�@�q@���@��[@��@�˒@��f@�]�@�8@��@���@��x@���@���@�@�@�M@�@E9@o@~�@~�R@~�h@~�A@~J@}[W@}�@}@@|�[@|j@|!@{��@z��@z�!@z?@z �@yF@x��@x	�@w��@w'�@v{�@uzx@t�5@t�@s�r@s��@s�@r�@rJ�@q�@qzx@q	l@p�5@p��@p��@p�@o�r@o�@o�q@oA�@o�@n��@n@�@m�Z@m��@l��@l�D@lj@l4n@k�&@k�:@kRT@k.I@j�2@j}V@i�>@i[W@h�f@h��@g�}@g�@gv`@g�@fi�@f�@e�3@eN<@d�[@dU2@c�@cF�@b�s@b��@b6�@a��@a*0@`��@_�F@_A�@^�,@^h
@^E�@^!�@^�@]�@]��@]o @][W@]5�@\z�@\1@[�&@Z�1@Z_�@Z�@Y��@YF@Y/@Y!�@X��@X�@W�@W��@W��@W+@V��@V�x@V3�@U��@U��@U/@T��@T��@Tm�@TM@S�@S1�@R��@R4@Q�9@Q��@Q*0@P��@P~@Oy�@O1�@O�@N�"@N�\@M�)@M�@M�@LɆ@L[�@Kخ@KW?@KS@J��@J�R@J\�@J3�@Ju@I�@IL�@H��@H��@HbN@H4n@H1@Gj�@G(@F�R@Fu%@Eϫ@E+�@E�@D��@C�}@C��@C��@C,�@B��@A�D@A��@AVm@A%F@A@@@��@@z�@@S�@@:�@@@?��@?�q@?|�@?Mj@>��@>��@>�@>��@>Ov@=�@=�@=��@=Dg@=8�@=�@<�?@<C-@;�]@;�@;��@;�f@;�@:�@:a|@:�@9�>@9�^@9u�@9q@8�p@8[�@8!@7� @7j�@7C@6͟@6��@6GE@6�@5��@5�h@5T�@4�@4��@4$@3��@3�@3iD@2�8@2{�@2=q@1�d@1Dg@0��@0/�@/�@/��@/b�@/H�@/&@.��@.��@.}V@.:*@.�@-�o@-��@-:�@,�@,~(@,H@,�@+�+@+�W@+ƨ@+X�@+�@+o@+�@*��@*�,@*��@*��@*i�@* �@)��@)o @)!�@(�K@(��@(M@'�m@'��@'��@'�{@'{J@'v`@'U�@'Y@'S@&��@&��@&\�@&+k@%�D@%�-@%�@%L�@$��@$��@$~(@$Ft@#x@#Y@"�@"�R@"v�@"ff@"J�@"8�@"8�@"5?@"�@!�z@!�'@!p�@!Vm@!4@ �?@ �@ V�@ <�@ 6@ ,=@ !@ 7@ �@��@��@F�@�@�@{�@C�@	@�@��@2a@�@�E@�@j@[�@4n@@��@y�@@O@&@'�@Y@@�@�@��@v�@�.@}�@4@*0@	l@�	@�5@�E@��@~@�@˒@�F@]�@@O@8@4�@��@@�@	@��@�9@��@X@�@֡@��@z�@U2@H@�@��@�*@��@o�@P�@1�@�@�"@ȴ@v�@&�@#:@+k@1�@3�@O@��@�^@�t@�S@f�@�P@��@��@��@�z@��@w�@r�@m�@<�@��@��@�g@��@�w@�:@4�@�@�@��@�r@xl@~�@��@�r@�A@h
@M�@�@�3@�@��@e,@F@+�@�@�@��@9X@�F@\)@;d@)_@1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  B`�BoOB~BBچBa-B�>B	;�B	\B	�;B	��B	��B	�+B	��B	��B	�B	�IB	�5B	�;B	�B	��B	�MB	��B	�AB	��B	��B	��B	�LB	��B	�XB	��B	��B	�iB	�@B	y�B	_pB	\B	e�B	mB	z*B	��B	��B	�=B	ңB	�B
#TB
1�B
5�B
��B
�)B
�TB
�6B
��B
�2B
�RB
�XB
��B
�B
�`B
��B
�B
z^B
p;B
f2B
NpB
VB
D3B
5�B
*eB
7�B
/�B	��B	��B	��B	��B	�B	shB	b4B	UgB	H�B	?�B	-�B	�B	�B	;B��B�vB��B�B�B��B�B�8B�zB��B�mB�B�!B�{B͟B��B�B�VB�B��B�JB�qB�'BȀBϫB��B��B	6B	mB	B	-�B	8B	?�B	G�B	O�B	QB	K�B	L~B	X�B	d@B	i�B	n�B	u�B	��B	�1B	�)B	��B	��B	�B	��B	�OB	ɆB	�	B	�rB	�HB	уB	��B	��B	�mB	οB	��B	ÖB	�gB	��B	�B	�9B	̘B	�JB	�<B	�NB	�}B	��B	��B	�B	ңB	�hB	�hB	��B	�$B	�KB	�EB	خB	�KB	�KB	�
B	�9B	ؓB	�)B	�BB	�BB	��B	��B	ބB	޸B	�]B	׍B	��B	�&B	�oB	�4B	��B	бB	�}B	ЗB	ЗB	�&B	��B	�9B	��B	�mB	��B	�B	յB	��B	ҽB	�uB	��B	ҽB	өB	յB	��B	��B	��B	��B	ؓB	��B	�B	��B	�B	�B	ּB	��B	ּB	֡B	��B	��B	өB	өB	��B	��B	�2B	ԕB	�aB	��B	�,B	�FB	�B	�aB	��B	��B	��B	�[B	�{B	өB	��B	�oB	�oB	� B	�B	҉B	�oB	��B	ңB	��B	�B	�&B	��B	��B	՛B	��B	��B	��B	�aB	�FB	ԯB	��B	��B	ՁB	�sB	��B	��B	��B	�sB	�?B	��B	רB	�+B	ؓB	�yB	خB	��B	�1B	��B	��B	ٴB	�KB	ٴB	��B	��B	�7B	�kB	�7B	�QB	�=B	��B	�)B	�B	�xB	�CB	�CB	�CB	�xB	ݘB	��B	�B	�B	�B	�VB	��B	�B	�;B	�pB	ߊB	߾B	�'B	�\B	�\B	�\B	�vB	��B	�bB	�4B	�hB	�B	��B	��B	��B	�B	�B	�B	�B	�,B	�B	�B	�mB	�8B	��B	�B	�yB	�_B	�0B	�B	�B	�B	��B	��B	�KB	�eB	�eB	�B	�B	��B	�WB	�)B	�B	��B	�B	�IB	��B	�B	�!B	��B	�'B	�[B	�[B	�B	�B	�B	�B	�B	��B	�B	�MB	�B	��B	�TB	�nB	�9B	�B	�%B	��B	��B	�tB	�tB	��B	��B	��B	��B	�zB	��B	�B	��B	�B	�B	�B	�8B	�B	��B	�RB	�	B	�	B	�XB	��B	�B	�DB	�*B	�B	�B	��B	��B	�6B	��B	��B	��B	��B	��B	��B	�.B	�.B	�HB	��B
 4B
 B
 �B
�B
B
uB
�B
�B
GB
{B
{B
aB
�B
MB
B
mB
�B
?B
�B
�B
+B
EB
�B
B
�B
�B
	B
�B
	�B
	�B

#B

�B

�B

�B
�B
dB
JB
JB
0B
JB
�B
B
�B
pB
�B
�B
\B
�B
bB
}B
�B
�B
�B
�B
B
 B
B
�B
�B
�B
�B
�B
�B
&B
�B
�B
�B
FB
�B
MB
�B
�B
�B
�B
sB
�B
�B
�B
_B
�B
�B
1B
B
B
�B
	B
�B
�B
)B
B
�B
)B
�B
/B
B
�B
B
�B
;B
;B
;B
!B
VB
�B
 vB
!-B
!�B
!�B
"B
"�B
#:B
$&B
$@B
$�B
%,B
%�B
%�B
'�B
)*B
)�B
)�B
*B
*B
+QB
-CB
-�B
-�B
.�B
/OB
/�B
/�B
/�B
/�B
0!B
0UB
0�B
0�B
1AB
1[B
1vB
1�B
2�B
3MB
3B
2�B
2�B
2�B
2�B
2|B
3�B
4TB
5%B
5ZB
5�B
5�B
5�B
6B
6B
5�B
6B
6FB
6�B
6�B
6�B
6�B
7B
7B
7fB
72B
7�B
7�B
7�B
7�B
72B
6�B
6�B
7�B
8B
7�B
7LB
7�B
7�B
72B
7�B
7�B
7�B
8RB
8RB
8lB
8lB
8�B
8�B
9rB
9�B
:B
9�B
9�B
9rB
9�B
9�B
9�B
:^B
:�B
:�B
:�B
;B
:�B
;0B
;dB
;�B
;�B
<�B
=<B
=�B
>BB
>�B
>�B
?HB
?�B
@�B
@�B
A;B
A�B
B'B
BuB
B�B
C-B
CGB
C�B
C�B
C�B
DB
DgB
D�B
E9B
E�B
E�B
F%B
F%B
F?B
F%B
FtB
F�B
F�B
F�B
F�B
GzB
GzB
GzB
H�B
H�B
H�B
IB
I7B
IB
IB
I�B
I�B
I�B
I�B
I�B
JrB
JrB
J�B
J�B
J�B
KB
K�B
KxB
K�B
K�B
LB
LdB
L~B
MB
MPB
M�B
M�B
M�B
N<B
NpB
N�B
OB
N�B
N�B
OBB
OvB
O�B
O�B
O�B
PbB
P�B
QB
QNB
QNB
Q�B
Q�B
Q�B
Q�B
R B
RoB
R�B
R�B
R�B
R�B
R�B
S[B
SuB
S�B
S�B
T�B
T�B
T�B
U2B
U�B
U�B
U�B
U�B
VB
V�B
V�B
V�B
W$B
W?B
W�B
XyB
X�B
X�B
X�B
X�B
X�B
YB
YB
YB
YeB
YeB
YKB
Y�B
ZB
ZB
Z7B
ZQB
ZQB
ZQB
Z�B
[	B
[#B
[#B
[=B
[WB
[�B
[�B
[�B
\B
\B
\)B
\CB
\xB
\�B
\�B
\�B
]IB
]~B
]�B
]�B
^B
^OB
^jB
^�B
^�B
^�B
_B
_;B
_�B
_�B
_�B
_�B
`'B
`\B
`vB
`�B
aB
a�B
bNB
b�B
b�B
b�B
b�B
cB
c B
c:B
c�B
c�B
c�B
c�B
dB
dZB
dtB
d�B
d�B
eFB
e`B
eFB
ezB
e�B
f2B
f2B
fB
f2B
fLB
f�B
f�B
f�B
gB
gB
g�B
g�B
h
B
h
B
h>B
g�B
h>B
h>B
hXB
hXB
hXB
h�B
iB
iB
i*B
i�B
j�B
kB
j�B
j�B
j�B
kB
k�B
k�B
k�B
lB
lqB
l�B
l�B
mCB
m]B
m]B
mwB
mwB
m]B
mwB
m�B
m�B
m�B
m�B
m�B
m�B
ncB
n�B
n�B
o5B
o5B
oOB
oiB
o�B
o�B
pB
p!B
poB
p�B
p�B
p�B
p�B
q'B
q'B
q[B
q�B
q�B
rGB
rGB
rGB
r-B
rB
r-B
r|B
sB
r�B
r�B
r�B
r�B
r�B
s3B
shB
sMB
shB
s�B
tB
s�B
s�B
s�B
tB
t9B
t�B
uZB
u�B
u�B
u�B
u�B
vB
u�B
u�B
u�B
u�B
u�B
u�B
v+B
v`B
v�B
v�B
w2B
w�B
w�B
w�B
w�B
w�B
w�B
x8B
x8B
x8B
xlB
xlB
x�B
x�B
x�B
x�B
y$B
y>B
yXB
yXB
y>B
y$B
yrB
zDB
z�B
zxB
z^B
z*B
z*B
y�B
y�B
y�B
y�B
zB
z*B
z^B
zxB
z�B
{�B
|B
|PB
|jB
|�B
|�B
|�B
|�B
}B
}qB
}�B
}�B
~B
~(B
~(B
~BB
~]B
~]B
~�B
~�B
B
HB
HB
}B
�B
�B
�B
��B
��B
� B
�oB
��B
��B
��1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  B`�BoOB~BBچBa-B�>B	;�B	\B	�;B	��B	��B	�+B	��B	��B	�B	�IB	�5B	�;B	�B	��B	�MB	��B	�AB	��B	��B	��B	�LB	��B	�XB	��B	��B	�iB	�@B	y�B	_pB	\B	e�B	mB	z*B	��B	��B	�=B	ңB	�B
#TB
1�B
5�B
��B
�)B
�TB
�6B
��B
�2B
�RB
�XB
��B
�B
�`B
��B
�B
z^B
p;B
f2B
NpB
VB
D3B
5�B
*eB
7�B
/�B	��B	��B	��B	��B	�B	shB	b4B	UgB	H�B	?�B	-�B	�B	�B	;B��B�vB��B�B�B��B�B�8B�zB��B�mB�B�!B�{B͟B��B�B�VB�B��B�JB�qB�'BȀBϫB��B��B	6B	mB	B	-�B	8B	?�B	G�B	O�B	QB	K�B	L~B	X�B	d@B	i�B	n�B	u�B	��B	�1B	�)B	��B	��B	�B	��B	�OB	ɆB	�	B	�rB	�HB	уB	��B	��B	�mB	οB	��B	ÖB	�gB	��B	�B	�9B	̘B	�JB	�<B	�NB	�}B	��B	��B	�B	ңB	�hB	�hB	��B	�$B	�KB	�EB	خB	�KB	�KB	�
B	�9B	ؓB	�)B	�BB	�BB	��B	��B	ބB	޸B	�]B	׍B	��B	�&B	�oB	�4B	��B	бB	�}B	ЗB	ЗB	�&B	��B	�9B	��B	�mB	��B	�B	յB	��B	ҽB	�uB	��B	ҽB	өB	յB	��B	��B	��B	��B	ؓB	��B	�B	��B	�B	�B	ּB	��B	ּB	֡B	��B	��B	өB	өB	��B	��B	�2B	ԕB	�aB	��B	�,B	�FB	�B	�aB	��B	��B	��B	�[B	�{B	өB	��B	�oB	�oB	� B	�B	҉B	�oB	��B	ңB	��B	�B	�&B	��B	��B	՛B	��B	��B	��B	�aB	�FB	ԯB	��B	��B	ՁB	�sB	��B	��B	��B	�sB	�?B	��B	רB	�+B	ؓB	�yB	خB	��B	�1B	��B	��B	ٴB	�KB	ٴB	��B	��B	�7B	�kB	�7B	�QB	�=B	��B	�)B	�B	�xB	�CB	�CB	�CB	�xB	ݘB	��B	�B	�B	�B	�VB	��B	�B	�;B	�pB	ߊB	߾B	�'B	�\B	�\B	�\B	�vB	��B	�bB	�4B	�hB	�B	��B	��B	��B	�B	�B	�B	�B	�,B	�B	�B	�mB	�8B	��B	�B	�yB	�_B	�0B	�B	�B	�B	��B	��B	�KB	�eB	�eB	�B	�B	��B	�WB	�)B	�B	��B	�B	�IB	��B	�B	�!B	��B	�'B	�[B	�[B	�B	�B	�B	�B	�B	��B	�B	�MB	�B	��B	�TB	�nB	�9B	�B	�%B	��B	��B	�tB	�tB	��B	��B	��B	��B	�zB	��B	�B	��B	�B	�B	�B	�8B	�B	��B	�RB	�	B	�	B	�XB	��B	�B	�DB	�*B	�B	�B	��B	��B	�6B	��B	��B	��B	��B	��B	��B	�.B	�.B	�HB	��B
 4B
 B
 �B
�B
B
uB
�B
�B
GB
{B
{B
aB
�B
MB
B
mB
�B
?B
�B
�B
+B
EB
�B
B
�B
�B
	B
�B
	�B
	�B

#B

�B

�B

�B
�B
dB
JB
JB
0B
JB
�B
B
�B
pB
�B
�B
\B
�B
bB
}B
�B
�B
�B
�B
B
 B
B
�B
�B
�B
�B
�B
�B
&B
�B
�B
�B
FB
�B
MB
�B
�B
�B
�B
sB
�B
�B
�B
_B
�B
�B
1B
B
B
�B
	B
�B
�B
)B
B
�B
)B
�B
/B
B
�B
B
�B
;B
;B
;B
!B
VB
�B
 vB
!-B
!�B
!�B
"B
"�B
#:B
$&B
$@B
$�B
%,B
%�B
%�B
'�B
)*B
)�B
)�B
*B
*B
+QB
-CB
-�B
-�B
.�B
/OB
/�B
/�B
/�B
/�B
0!B
0UB
0�B
0�B
1AB
1[B
1vB
1�B
2�B
3MB
3B
2�B
2�B
2�B
2�B
2|B
3�B
4TB
5%B
5ZB
5�B
5�B
5�B
6B
6B
5�B
6B
6FB
6�B
6�B
6�B
6�B
7B
7B
7fB
72B
7�B
7�B
7�B
7�B
72B
6�B
6�B
7�B
8B
7�B
7LB
7�B
7�B
72B
7�B
7�B
7�B
8RB
8RB
8lB
8lB
8�B
8�B
9rB
9�B
:B
9�B
9�B
9rB
9�B
9�B
9�B
:^B
:�B
:�B
:�B
;B
:�B
;0B
;dB
;�B
;�B
<�B
=<B
=�B
>BB
>�B
>�B
?HB
?�B
@�B
@�B
A;B
A�B
B'B
BuB
B�B
C-B
CGB
C�B
C�B
C�B
DB
DgB
D�B
E9B
E�B
E�B
F%B
F%B
F?B
F%B
FtB
F�B
F�B
F�B
F�B
GzB
GzB
GzB
H�B
H�B
H�B
IB
I7B
IB
IB
I�B
I�B
I�B
I�B
I�B
JrB
JrB
J�B
J�B
J�B
KB
K�B
KxB
K�B
K�B
LB
LdB
L~B
MB
MPB
M�B
M�B
M�B
N<B
NpB
N�B
OB
N�B
N�B
OBB
OvB
O�B
O�B
O�B
PbB
P�B
QB
QNB
QNB
Q�B
Q�B
Q�B
Q�B
R B
RoB
R�B
R�B
R�B
R�B
R�B
S[B
SuB
S�B
S�B
T�B
T�B
T�B
U2B
U�B
U�B
U�B
U�B
VB
V�B
V�B
V�B
W$B
W?B
W�B
XyB
X�B
X�B
X�B
X�B
X�B
YB
YB
YB
YeB
YeB
YKB
Y�B
ZB
ZB
Z7B
ZQB
ZQB
ZQB
Z�B
[	B
[#B
[#B
[=B
[WB
[�B
[�B
[�B
\B
\B
\)B
\CB
\xB
\�B
\�B
\�B
]IB
]~B
]�B
]�B
^B
^OB
^jB
^�B
^�B
^�B
_B
_;B
_�B
_�B
_�B
_�B
`'B
`\B
`vB
`�B
aB
a�B
bNB
b�B
b�B
b�B
b�B
cB
c B
c:B
c�B
c�B
c�B
c�B
dB
dZB
dtB
d�B
d�B
eFB
e`B
eFB
ezB
e�B
f2B
f2B
fB
f2B
fLB
f�B
f�B
f�B
gB
gB
g�B
g�B
h
B
h
B
h>B
g�B
h>B
h>B
hXB
hXB
hXB
h�B
iB
iB
i*B
i�B
j�B
kB
j�B
j�B
j�B
kB
k�B
k�B
k�B
lB
lqB
l�B
l�B
mCB
m]B
m]B
mwB
mwB
m]B
mwB
m�B
m�B
m�B
m�B
m�B
m�B
ncB
n�B
n�B
o5B
o5B
oOB
oiB
o�B
o�B
pB
p!B
poB
p�B
p�B
p�B
p�B
q'B
q'B
q[B
q�B
q�B
rGB
rGB
rGB
r-B
rB
r-B
r|B
sB
r�B
r�B
r�B
r�B
r�B
s3B
shB
sMB
shB
s�B
tB
s�B
s�B
s�B
tB
t9B
t�B
uZB
u�B
u�B
u�B
u�B
vB
u�B
u�B
u�B
u�B
u�B
u�B
v+B
v`B
v�B
v�B
w2B
w�B
w�B
w�B
w�B
w�B
w�B
x8B
x8B
x8B
xlB
xlB
x�B
x�B
x�B
x�B
y$B
y>B
yXB
yXB
y>B
y$B
yrB
zDB
z�B
zxB
z^B
z*B
z*B
y�B
y�B
y�B
y�B
zB
z*B
z^B
zxB
z�B
{�B
|B
|PB
|jB
|�B
|�B
|�B
|�B
}B
}qB
}�B
}�B
~B
~(B
~(B
~BB
~]B
~]B
~�B
~�B
B
HB
HB
}B
�B
�B
�B
��B
��B
� B
�oB
��B
��B
��1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                            JA  ARFMdecpA30a                                                                20220604104945  IP                  G�O�G�O�G�O�                JA  ARFMfmtp2.8                                                                 20220604174856  IP                  G�O�G�O�G�O�                JA  ARCAsspa3.1b                                                                20220604174856  IP  PRES            G�O�G�O�G�O�                JA      jafc1.0                                                                 20220604174856                      G�O�G�O�G�O�                JA  ARGQrqcpc3.6                                                                20220605024903  QCP$                G�O�G�O�G�O�         20DF37EJA  ARGQrqcpc3.6                                                                20220605024903  QCF$                G�O�G�O�G�O�               0JA  ARUP                                                                        20220610141505                      G�O�G�O�G�O�                