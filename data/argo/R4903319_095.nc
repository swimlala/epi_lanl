CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       2022-11-15T11:01:52Z creation      
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
resolution        =���   axis      Z        �  9p   PRES_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  I   PRES_ADJUSTED            
      	   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  L�   PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  \�   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         decibar    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  `�   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  p    TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  ��   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �H   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         degree_Celsius     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  �0   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  ��   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �p   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  �X   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ��   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  ��   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  0  ݀   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                    ݰ   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                    �   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                    �   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  ,  �   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                    ��   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                    ��   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                    ��   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                    ��   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  @  ��   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                    �,   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                    �<   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    �@   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar         �P   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar         �T   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�        �X   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    �\Argo profile    3.1 1.2 19500101000000  20221115110152  20221115110152  4903319 Argo PMEL                                                       GREGORY C. JOHNSON                                              PRES            TEMP            PSAL               _A   AO  8280                            2B  A   NAVIS_A                         1159                            170425                          863 @��L�N'1   @�����F@:F�x����d��7Kƨ1   GPS     Primary sampling: averaged                                                                                                                                                                                                                                         _A   A   A   @�33@�  @���AffA@  A`  A�  A�  A�  A�33A�  A�  A�  A�  B   B  B  B  B��B(  B0  B8  B@  BH  BP  BX  B`  Bh  Bp  Bx  B�  B�  B�  B�33B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  C   C  C  C  C  C
  C  C  C  C  C  C  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dt� Du  Du� Dv  Dv� Dw  Dw� Dx  Dx� Dy  Dy� Dz  Dz� D{  D{� D|  D|� D}  D}� D~  D~� D  D� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D���D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D D�� D�  D�@ DÀ D�� D�  D�@ DĀ D�� D�  D�@ Dŀ D�� D�  D�@ Dƀ D�� D�  D�@ Dǀ D�� D�  D�@ DȀ D�� D�  D�@ Dɀ D�� D�  D�@ Dʀ D�� D�  D�@ Dˀ D�� D�  D�@ D̀ D�� D�  D�@ D̀ D�� D�  D�@ D΀ D�� D�  D�@ Dπ D�� D�  D�@ DЀ D�� D�  D�@ Dр D�� D�  D�@ DҀ D�� D�  D�@ DӀ D�� D�  D�@ DԀ D�� D�  D�@ DՀ D�� D�  D�@ Dր D�� D�  D�@ D׀ D�� D�  D�@ D؀ D�� D�  D�@ Dـ D�� D�  D�@ Dڀ D�� D�  D�@ Dۀ D�� D�  D�@ D܀ D�� D�  D�@ D݀ D�� D�  D�@ Dހ D�� D�  D�@ D߀ D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�I�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111@�ff@�33A   A   AA��Aa��A���A���A���A�  A���A���A���A���B ffBffBffBffB   B(ffB0ffB8ffB@ffBHffBPffBXffB`ffBhffBpffBxffB�33B�33B�33B�ffB�33B�33B�33B�33B�33B�33B�33B�33B�33B�33B�33B�33B�33B�33B�33B�33B�33B�33B�33B�33B�33B�33B�33B�33B�33B�33B�33B�33C �C�C�C�C�C
�C�C�C�C�C�C�C�C�C�C�C �C"�C$�C&�C(�C*�C,�C.�C0�C2�C4�C6�C8�C:�C<�C>�C@�CB�CD�CF�CH�CJ�CL�CN�CP�CR�CT�CV�CX�CZ�C\�C^�C`�Cb�Cd�Cf�Ch�Cj�Cl�Cn�Cp�Cr�Ct�Cv�Cx�Cz�C|�C~�C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C�  C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��D fD �fDfD�fDfD�fDfD�fDfD�fDfD�fDfD�fDfD�fDfD�fD	fD	�fD
fD
�fDfD�fDfD�fDfD�fDfD�fDfD�fDfD�fDfD�fDfD�fDfD�fDfD�fDfD�fDfD�fDfD�fDfD�fDfD�fDfD�fDfD�fDfD�fDfD�fDfD�fDfD�fD fD �fD!fD!�fD"fD"�fD#fD#�fD$fD$�fD%fD%�fD&fD&�fD'fD'�fD(fD(�fD)fD)�fD*fD*�fD+fD+�fD,fD,�fD-fD-�fD.fD.�fD/fD/�fD0fD0�fD1fD1�fD2fD2�fD3fD3�fD4fD4�fD5fD5�fD6fD6�fD7fD7�fD8fD8�fD9fD9�fD:fD:�fD;fD;�fD<fD<�fD=fD=�fD>fD>�fD?fD?�fD@fD@�fDAfDA�fDBfDB�fDCfDC�fDDfDD�fDEfDE�fDFfDF�fDGfDG�fDHfDH�fDIfDI�fDJfDJ�fDKfDK�fDLfDL�fDMfDM�fDNfDN�fDOfDO�fDPfDP�fDQfDQ�fDRfDR�fDSfDS�fDTfDT�fDUfDU�fDVfDV�fDWfDW�fDXfDX�fDYfDY�fDZfDZ�fD[fD[�fD\fD\�fD]fD]�fD^fD^�fD_fD_�fD`fD`�fDafDa�fDbfDb�fDcfDc�fDdfDd�fDefDe�fDffDf�fDgfDg�fDhfDh�fDifDi�fDjfDj�fDkfDk�fDlfDl�fDmfDm�fDnfDn�fDofDo�fDpfDp�fDqfDq�fDrfDr�fDsfDs�fDtfDt�fDufDu�fDvfDv�fDwfDw�fDxfDx�fDyfDy�fDzfDz�fD{fD{�fD|fD|�fD}fD}�fD~fD~�fDfD�fD�3D�C3D��3D��3D�3D�C3D��3D��3D�3D�C3D��3D��3D�3D�C3D��3D��3D�3D�C3D��3D��3D�3D�C3D��3D��3D�3D�C3D��3D��3D�3D�C3D��3D��3D�3D�C3D��3D��3D�3D�C3D��3D��3D�3D�C3D��3D��3D�3D�C3D��3D��3D�3D�C3D��3D��3D�3D�C3D��3D��3D�3D�C3D��3D��3D�3D�C3D��3D��3D�3D�C3D��3D��3D�3D�C3D��3D��3D�3D�C3D��3D��3D�3D�C3D��3D��3D�3D�C3D��3D��3D�3D�C3D��3D��3D�3D�C3D��3D��3D�3D�C3D��3D��3D�3D�C3D��3D��3D�3D�C3D��3D��3D�3D�C3D��3D��3D�3D�C3D��3D��3D�3D�C3D��3D��3D�3D�C3D��3D��3D�3D�C3D��3D��3D�3D�C3D��3D��3D�3D�C3D��3D��3D�3D�C3D��3D��3D�3D�C3D��3D��3D�3D�C3D��3D��3D�3D�C3D��3D��3D�3D�C3D��3D��3D�3D�C3D��3D��3D�3D�C3D��3D��3D�3D�C3D��3D��3D�3D�C3D��3D��3D�3D�C3D��3D��3D�3D�C3D��3D��3D�3D�C3D��3D��3D�  D�C3D��3D��3D�3D�C3D��3D��3D�3D�C3D��3D��3D�3D�C3D��3D��3D�3D�C3D��3D��3D�3D�C3D��3D��3D�3D�C3D��3D��3D�3D�C3D��3D��3D�3D�C3D��3D��3D�3D�C3D��3D��3D�3D�C3D��3D��3D�3D�C3D��3D��3D�3D�C3D��3D��3D�3D�C3D��3D��3D�3D�C3D��3D��3D�3D�C3D��3D��3D�3D�C3D��3D��3D�3D�C3D��3D��3D�3D�C3D��3D��3D�3D�C3D��3D��3D�3D�C3D��3D��3D�3D�C3D3D��3D�3D�C3DÃ3D��3D�3D�C3Dă3D��3D�3D�C3DŃ3D��3D�3D�C3Dƃ3D��3D�3D�C3Dǃ3D��3D�3D�C3Dȃ3D��3D�3D�C3DɃ3D��3D�3D�C3Dʃ3D��3D�3D�C3D˃3D��3D�3D�C3D̃3D��3D�3D�C3D̓3D��3D�3D�C3D΃3D��3D�3D�C3Dσ3D��3D�3D�C3DЃ3D��3D�3D�C3Dу3D��3D�3D�C3D҃3D��3D�3D�C3DӃ3D��3D�3D�C3Dԃ3D��3D�3D�C3DՃ3D��3D�3D�C3Dփ3D��3D�3D�C3D׃3D��3D�3D�C3D؃3D��3D�3D�C3Dك3D��3D�3D�C3Dڃ3D��3D�3D�C3Dۃ3D��3D�3D�C3D܃3D��3D�3D�C3D݃3D��3D�3D�C3Dރ3D��3D�3D�C3D߃3D��3D�3D�C3D��3D��3D�3D�C3D�3D��3D�3D�C3D�3D��3D�3D�C3D�3D��3D�3D�C3D�3D��3D�3D�C3D�3D��3D�3D�C3D�3D��3D�3D�C3D�3D��3D�3D�C3D�3D��3D�3D�C3D�3D��3D�3D�C3D�3D��3D�3D�C3D�3D��3D�3D�C3D�3D��3D�3D�C3D�3D��3D�3D�C3D�3D��3D�3D�C3D�3D��3D�3D�C3D��3D��3D�3D�C3D�3D��3D�3D�C3D�3D��3D�3D�C3D�3D��3D�3D�C3D�3D��3D�3D�C3D��3D��3D�3D�C3D��3D��3D�3D�C3D��3D��3D�3D�C3D��3D��3D�3D�C3D��3D��3D�3D�L�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�A�z�AʅAʇ+AʅAʁAʃAʇ+Aʏ\AʑhAʑhAʏ\AʑhAʑhAʑhAʏ\Aʕ�Aʗ�Aʙ�Aʗ�Aʗ�Aʗ�AʓuAʃAʁAʃAʍPAʕ�AʓuAʉ7A�z�A�dZA�JA���A���A�ZA���A�ZA���A��A��/A���A�I�A���A�XA�K�A�\)A��A���A�z�A��A�A�A�%A���A�9XA��A��A��/A��A�A�$�A��`A���A�ȴA��^A��PA���A��!A���A��uA���A�v�A��`A���A���A��PA�ZA�(�A�`BA�x�A�+A�S�A�ĜA�ZA�(�A���A��A�VA�VA�A��-A�A�A&�A~~�A}�wA}VA{�wAzz�AwhsAt�AtbArI�Ap�`Ao��Ao/Am��Amp�Am\)Am&�AlZAjJAg�PAd��Ad��AdA�AcAb�yAb�DAb�Ab-AaO�A_�
A^v�A^M�A]hsA\��A[O�AZ��AZr�AY%AW;dAU�PATE�AS/ARAPZAN~�AM�AK��AJz�AIK�AH�`AG�
AGl�AGC�AF�`AE��ADI�AA�A?�
A>��A=p�A<bA;p�A:�HA:bA9�A9��A8^5A7\)A6 �A5�hA5/A4�yA4�\A3�FA1�7A0�HA0��A/�A/G�A.��A.�!A.E�A-��A-�-A-x�A,�A*n�A)�A)K�A(��A'ƨA'+A&ZA%?}A$I�A#"�A"^5A"$�A!��A!�^A!��A!G�A �/A JA\)A9XA�A;dAbA`BA�/A=qA�AVA��A��A�AE�A��A&�A�A�-AK�A+A�A��A�AS�A�^AA
ĜA
�A5?A�AZA�A�TA�hA�/A^5A��AK�A �jA �@��@���@���@�r�@��F@��-@���@�{@�9X@�@�u@���@�C�@�@���@��T@�33@�E�@�%@�F@�n�@�-@�O�@��`@�A�@��T@��
@�@ڏ\@ٙ�@�1@Ձ@���@���@���@���@Լj@ԋD@�j@ӥ�@ҟ�@�?}@�@�&�@�r�@��m@���@ɲ-@��@���@�ff@���@�p�@Ĵ9@�r�@�5?@� �@��F@�t�@��@�V@��/@���@��@��;@��y@�O�@���@��u@�Q�@�b@��@��
@��w@��@�l�@��@���@���@�=q@��#@��-@���@��h@��@��@�?}@��@��`@��@�9X@��m@�33@��@���@�b@���@�\)@��@�$�@���@�O�@���@��@���@�^5@��T@���@���@�(�@�t�@�@��!@�n�@�@���@��h@��@�Z@�1@��w@��@�$�@�J@��#@��@�I�@�  @�ƨ@���@�t�@�33@�l�@�o@���@���@��\@��@��@��^@�O�@��j@��@��m@��;@���@�33@�@���@���@�~�@��@���@���@���@�X@�&�@���@��D@�1'@�  @���@���@���@��@�dZ@�K�@��@��y@��R@��+@�ff@�M�@�5?@�{@��T@��@�-@���@���@���@��7@��/@��9@�1'@��@�C�@�o@���@���@�5?@�@���@��@�G�@���@��@�r�@�Q�@�A�@��@��;@�t�@�+@�@��@��@��\@�V@��@���@�/@��j@�r�@�A�@�9X@�z�@�9X@�ƨ@��@�dZ@�+@�"�@���@���@���@���@��@��y@��y@���@��!@�ȴ@��@��!@�M�@�J@��@�@��7@�`B@�/@��@��@���@�z�@�Q�@~��@~��@~�@~ȴ@~�+@}�@}�h@|j@|1@z�@z-@y��@y�7@yX@y�@x�9@x�u@x�@x1'@w|�@w+@vv�@u��@t�D@tI�@t(�@s�m@s�F@sS�@r�H@r��@r�!@r�\@r-@rJ@q�#@q��@qhs@q&�@p��@p��@p1'@o�P@o;d@o\)@o\)@oK�@n��@nff@m@m�@m`B@l��@l�@k��@kt�@kC�@k"�@j�H@j�!@j~�@j=q@i��@i��@i7L@h��@h�9@hbN@h1'@hb@g��@g|�@g;d@g;d@g
=@f��@f�y@fȴ@fv�@f5?@f@e@e/@d�/@d�j@dj@d�@d1@c��@cƨ@ct�@co@b��@bM�@a��@a��@a7L@`Ĝ@`Q�@_��@_�@_�@^v�@^E�@^@]/@\�@\1@[��@[dZ@["�@Z��@Z�!@Z�\@Z^5@Z-@Y��@Y�^@Y�@X��@Xr�@X1'@W�w@WK�@V�R@V��@Vv�@V{@U��@T��@T�D@TZ@T9X@S�F@SdZ@R�@R-@Q��@Q��@P�`@PA�@O��@O��@O�P@Ol�@O;d@O�@Nȴ@Nff@N{@M��@M�h@M?}@L��@L��@L9X@K��@K��@KC�@Ko@K@J�H@J~�@I��@I��@IX@I%@H�`@HĜ@H��@HA�@G�@G�;@G�w@G;d@F�y@F�R@F��@Fv�@E��@E@E�h@D�@D�D@D�D@D�D@DZ@D1@CdZ@C@B��@B^5@B�@A�#@Ax�@@��@@��@@r�@@b@?�@?��@?��@?�@>��@>�+@>$�@=��@=V@<z�@<9X@<�@;��@;@:�!@:~�@:n�@:-@9��@9�7@9hs@9&�@9�@9&�@8��@8�9@8r�@8 �@8b@7�P@7K�@6��@6�R@65?@5�-@5�@5?}@5V@4��@4j@3�
@3�@3S�@3"�@3o@3@2��@2M�@2=q@1�^@1G�@1�@0��@0Ĝ@0A�@/�@/��@/|�@/+@.��@.�y@.�@.�@.ff@.5?@.{@.{@-�-@-O�@-V@,�/@,j@+��@+��@+t�@+33@*��@)��@)G�@(Ĝ@(1'@'��@'+@'
=@&�@&ȴ@&��@&V@&E�@&@%@%�@%?}@%/@%/@%/@$�/@$�D@$j@$j@$j@$j@$Z@$�@#��@#@"��@"n�@"n�@"M�@"J@!�@!��@!��@!��@!x�@!hs@!hs@!G�@!�@ �9@ r�@ A�@ b@�@�w@��@l�@
=@�@ȴ@ȴ@�R@�R@�R@�+@V@E�@E�@5?@$�@�T@�-@�@�@�/@��@�D@9X@�m@o@�H@��@�\@-@��@�^@hs@X@G�@�@%@%@��@Ĝ@�u@�@r�@Q�@  @|�@K�@�@ȴ@��@V@$�@$�@�@�h@�@�D@Z@I�@I�@�@��@�m@�
@ƨ@��@�@S�@�H@��@�!@��@M�@��@�#@�^@�^@�^@��@G�@%@��@�u@�u@bN@Q�@Q�@A�@A�@1'@b@�@��@|�@;d@;d@;d@+@+@
=@��@�@��@5?@@��@�h@O�@/@��@��@��@z�@I�@9X@(�@1@�m@�F@t�@t�@t�@33@
�H@
�H@
��@
��@
~�@
^5@
M�@
M�@
=q@
-@	��@	�@	�#@	x�@	hs@	hs@	hs@	G�@	&�@��@��@��@Ĝ@Ĝ@�9@��@�u@�@r�@bN@Q�@  @�@�P@K�@;d@�@�@��@�+@v�@V@5?@{@�T@@��@p�@p�@`B@?}@��@�/@�/@�j@z�@9X@1@��@��@ƨ@��@��@�@S�@33@"�@�@��@��@��1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111A�z�AʅAʇ+AʅAʁAʃAʇ+Aʏ\AʑhAʑhAʏ\AʑhAʑhAʑhAʏ\Aʕ�Aʗ�Aʙ�Aʗ�Aʗ�Aʗ�AʓuAʃAʁAʃAʍPAʕ�AʓuAʉ7A�z�A�dZA�JA���A���A�ZA���A�ZA���A��A��/A���A�I�A���A�XA�K�A�\)A��A���A�z�A��A�A�A�%A���A�9XA��A��A��/A��A�A�$�A��`A���A�ȴA��^A��PA���A��!A���A��uA���A�v�A��`A���A���A��PA�ZA�(�A�`BA�x�A�+A�S�A�ĜA�ZA�(�A���A��A�VA�VA�A��-A�A�A&�A~~�A}�wA}VA{�wAzz�AwhsAt�AtbArI�Ap�`Ao��Ao/Am��Amp�Am\)Am&�AlZAjJAg�PAd��Ad��AdA�AcAb�yAb�DAb�Ab-AaO�A_�
A^v�A^M�A]hsA\��A[O�AZ��AZr�AY%AW;dAU�PATE�AS/ARAPZAN~�AM�AK��AJz�AIK�AH�`AG�
AGl�AGC�AF�`AE��ADI�AA�A?�
A>��A=p�A<bA;p�A:�HA:bA9�A9��A8^5A7\)A6 �A5�hA5/A4�yA4�\A3�FA1�7A0�HA0��A/�A/G�A.��A.�!A.E�A-��A-�-A-x�A,�A*n�A)�A)K�A(��A'ƨA'+A&ZA%?}A$I�A#"�A"^5A"$�A!��A!�^A!��A!G�A �/A JA\)A9XA�A;dAbA`BA�/A=qA�AVA��A��A�AE�A��A&�A�A�-AK�A+A�A��A�AS�A�^AA
ĜA
�A5?A�AZA�A�TA�hA�/A^5A��AK�A �jA �@��@���@���@�r�@��F@��-@���@�{@�9X@�@�u@���@�C�@�@���@��T@�33@�E�@�%@�F@�n�@�-@�O�@��`@�A�@��T@��
@�@ڏ\@ٙ�@�1@Ձ@���@���@���@���@Լj@ԋD@�j@ӥ�@ҟ�@�?}@�@�&�@�r�@��m@���@ɲ-@��@���@�ff@���@�p�@Ĵ9@�r�@�5?@� �@��F@�t�@��@�V@��/@���@��@��;@��y@�O�@���@��u@�Q�@�b@��@��
@��w@��@�l�@��@���@���@�=q@��#@��-@���@��h@��@��@�?}@��@��`@��@�9X@��m@�33@��@���@�b@���@�\)@��@�$�@���@�O�@���@��@���@�^5@��T@���@���@�(�@�t�@�@��!@�n�@�@���@��h@��@�Z@�1@��w@��@�$�@�J@��#@��@�I�@�  @�ƨ@���@�t�@�33@�l�@�o@���@���@��\@��@��@��^@�O�@��j@��@��m@��;@���@�33@�@���@���@�~�@��@���@���@���@�X@�&�@���@��D@�1'@�  @���@���@���@��@�dZ@�K�@��@��y@��R@��+@�ff@�M�@�5?@�{@��T@��@�-@���@���@���@��7@��/@��9@�1'@��@�C�@�o@���@���@�5?@�@���@��@�G�@���@��@�r�@�Q�@�A�@��@��;@�t�@�+@�@��@��@��\@�V@��@���@�/@��j@�r�@�A�@�9X@�z�@�9X@�ƨ@��@�dZ@�+@�"�@���@���@���@���@��@��y@��y@���@��!@�ȴ@��@��!@�M�@�J@��@�@��7@�`B@�/@��@��@���@�z�@�Q�@~��@~��@~�@~ȴ@~�+@}�@}�h@|j@|1@z�@z-@y��@y�7@yX@y�@x�9@x�u@x�@x1'@w|�@w+@vv�@u��@t�D@tI�@t(�@s�m@s�F@sS�@r�H@r��@r�!@r�\@r-@rJ@q�#@q��@qhs@q&�@p��@p��@p1'@o�P@o;d@o\)@o\)@oK�@n��@nff@m@m�@m`B@l��@l�@k��@kt�@kC�@k"�@j�H@j�!@j~�@j=q@i��@i��@i7L@h��@h�9@hbN@h1'@hb@g��@g|�@g;d@g;d@g
=@f��@f�y@fȴ@fv�@f5?@f@e@e/@d�/@d�j@dj@d�@d1@c��@cƨ@ct�@co@b��@bM�@a��@a��@a7L@`Ĝ@`Q�@_��@_�@_�@^v�@^E�@^@]/@\�@\1@[��@[dZ@["�@Z��@Z�!@Z�\@Z^5@Z-@Y��@Y�^@Y�@X��@Xr�@X1'@W�w@WK�@V�R@V��@Vv�@V{@U��@T��@T�D@TZ@T9X@S�F@SdZ@R�@R-@Q��@Q��@P�`@PA�@O��@O��@O�P@Ol�@O;d@O�@Nȴ@Nff@N{@M��@M�h@M?}@L��@L��@L9X@K��@K��@KC�@Ko@K@J�H@J~�@I��@I��@IX@I%@H�`@HĜ@H��@HA�@G�@G�;@G�w@G;d@F�y@F�R@F��@Fv�@E��@E@E�h@D�@D�D@D�D@D�D@DZ@D1@CdZ@C@B��@B^5@B�@A�#@Ax�@@��@@��@@r�@@b@?�@?��@?��@?�@>��@>�+@>$�@=��@=V@<z�@<9X@<�@;��@;@:�!@:~�@:n�@:-@9��@9�7@9hs@9&�@9�@9&�@8��@8�9@8r�@8 �@8b@7�P@7K�@6��@6�R@65?@5�-@5�@5?}@5V@4��@4j@3�
@3�@3S�@3"�@3o@3@2��@2M�@2=q@1�^@1G�@1�@0��@0Ĝ@0A�@/�@/��@/|�@/+@.��@.�y@.�@.�@.ff@.5?@.{@.{@-�-@-O�@-V@,�/@,j@+��@+��@+t�@+33@*��@)��@)G�@(Ĝ@(1'@'��@'+@'
=@&�@&ȴ@&��@&V@&E�@&@%@%�@%?}@%/@%/@%/@$�/@$�D@$j@$j@$j@$j@$Z@$�@#��@#@"��@"n�@"n�@"M�@"J@!�@!��@!��@!��@!x�@!hs@!hs@!G�@!�@ �9@ r�@ A�@ b@�@�w@��@l�@
=@�@ȴ@ȴ@�R@�R@�R@�+@V@E�@E�@5?@$�@�T@�-@�@�@�/@��@�D@9X@�m@o@�H@��@�\@-@��@�^@hs@X@G�@�@%@%@��@Ĝ@�u@�@r�@Q�@  @|�@K�@�@ȴ@��@V@$�@$�@�@�h@�@�D@Z@I�@I�@�@��@�m@�
@ƨ@��@�@S�@�H@��@�!@��@M�@��@�#@�^@�^@�^@��@G�@%@��@�u@�u@bN@Q�@Q�@A�@A�@1'@b@�@��@|�@;d@;d@;d@+@+@
=@��@�@��@5?@@��@�h@O�@/@��@��@��@z�@I�@9X@(�@1@�m@�F@t�@t�@t�@33@
�H@
�H@
��@
��@
~�@
^5@
M�@
M�@
=q@
-@	��@	�@	�#@	x�@	hs@	hs@	hs@	G�@	&�@��@��@��@Ĝ@Ĝ@�9@��@�u@�@r�@bN@Q�@  @�@�P@K�@;d@�@�@��@�+@v�@V@5?@{@�T@@��@p�@p�@`B@?}@��@�/@�/@�j@z�@9X@1@��@��@ƨ@��@��@�@S�@33@"�@�@��@��@��1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�B~�B~�B~�B~�B� B� B~�B~�B~�B~�B~�B~�B~�B~�B~�B~�B� B� B� B� B� B~�B}�B|�B}�B}�B~�B}�B}�B{�Bw�Bm�B0!B��B��B� Bs�Bm�BffBZBJ�B=qB6FB/B$�B�B��B�B�fB��BɺBǮB�XB�!B��B��B�uB�Bo�B^5BYBW
BVBS�BN�B=qB�BDB
��B
�B
�B
�NB
��B
ƨB
�3B
�!B
�B
��B
��B
�hB
�=B
�B
|�B
y�B
w�B
s�B
e`B
\)B
XB
S�B
N�B
E�B
@�B
;dB
6FB
.B
)�B
�B
PB
bB
+B	��B	��B	�B	�yB	�mB	�fB	�TB	�5B	��B	ŢB	�?B	�'B	�B	�B	��B	�B	�'B	�-B	�B	��B	��B	��B	��B	�bB	�7B	�B	� B	v�B	k�B	bNB	]/B	W
B	N�B	E�B	8RB	.B	#�B	�B	uB	bB	PB	JB	VB	oB	PB	B��B�B�mB�ZB�/B�#B�B��B��B��B��BȴBŢBÖBB��B�}B�wB�LB�3B�3B�3B�!B�B�B�B��B��B��B��B��B��B��B��B�uB�bB�JB�=B�B�B~�B}�B|�B{�B{�By�Bw�Bu�Bs�Bp�Bn�Bm�BjBgmBffBdZB`BB]/B[#BVBR�BP�BO�BM�BL�BK�BH�BG�BF�BC�B@�B=qB:^B9XB7LB5?B33B/B/B.B-B-B+B(�B'�B%�B%�B!�B!�B �B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B{BuBuBuBuBoBoB{BoBoBhBhBbB{BuBuBuBoBoBoBhBoBhB{B�B�B{B�B�B�B�B�B�B�B�B�B�B�B�B�B!�B$�B#�B#�B(�B,B-B/B2-B33B49B5?B6FB7LB8RB9XB:^B=qBE�BH�BK�BM�BO�BP�BQ�BQ�BQ�BQ�BS�BS�BT�BVBW
BW
BYB_;BcTBe`BffBgmBiyBk�Bm�Bn�Bo�Bw�B{�B|�B~�B�B�B�%B�7B�JB�VB�\B�oB�uB�uB��B��B��B��B��B��B��B��B��B�B�!B�!B�'B�-B�9B�XB�qB��B��BBǮBȴB��B��B��B�B�B�B�#B�;B�HB�TB�ZB�fB�B�B�B�B��B��B��B	B	%B		7B		7B	DB	bB	uB	�B	�B	�B	�B	�B	�B	�B	�B	�B	!�B	#�B	$�B	)�B	,B	1'B	33B	49B	7LB	9XB	<jB	=qB	A�B	C�B	C�B	D�B	G�B	I�B	M�B	O�B	P�B	R�B	VB	W
B	XB	YB	[#B	^5B	aHB	cTB	e`B	ffB	gmB	hsB	iyB	k�B	m�B	o�B	r�B	t�B	v�B	x�B	{�B	|�B	}�B	� B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�%B	�DB	�VB	�\B	�bB	�hB	�oB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�'B	�3B	�?B	�FB	�LB	�RB	�XB	�XB	�XB	�^B	�jB	�jB	�wB	�}B	B	ÖB	ÖB	ĜB	ĜB	ƨB	ǮB	ǮB	ȴB	ȴB	ɺB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�B	�B	�#B	�)B	�)B	�/B	�/B	�/B	�5B	�;B	�;B	�HB	�NB	�TB	�ZB	�`B	�`B	�fB	�fB	�fB	�fB	�yB	�yB	�yB	�yB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
  B
  B
  B
B
B
B
B
B
B
B
B
B
B
%B
%B
+B
1B
	7B
	7B
	7B
	7B

=B
DB
JB
PB
PB
VB
\B
bB
bB
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
{B
{B
{B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
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
!�B
"�B
"�B
"�B
#�B
#�B
$�B
%�B
%�B
%�B
&�B
&�B
&�B
&�B
'�B
'�B
(�B
(�B
(�B
)�B
+B
+B
+B
+B
,B
-B
-B
-B
.B
.B
.B
.B
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
1'B
1'B
2-B
33B
33B
33B
49B
49B
49B
5?B
6FB
6FB
6FB
6FB
7LB
7LB
7LB
7LB
8RB
9XB
9XB
9XB
9XB
:^B
:^B
:^B
:^B
:^B
:^B
:^B
:^B
:^B
:^B
:^B
;dB
<jB
=qB
=qB
=qB
=qB
>wB
>wB
>wB
>wB
?}B
@�B
@�B
@�B
@�B
A�B
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
E�B
E�B
F�B
F�B
F�B
F�B
F�B
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
I�B
I�B
I�B
I�B
I�B
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
M�B
M�B
M�B
M�B
M�B
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
O�B
O�B
O�B
P�B
P�B
P�B
R�B
R�B
R�B
R�B
R�B
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
ZB
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
ffB
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
l�B
l�B
l�B
m�B
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
q�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111B~�B~�B~�B~�B� B� B~�B~�B~�B~�B~�B~�B~�B~�B~�B~�B� B� B� B� B� B~�B}�B|�B}�B}�B~�B}�B}�B{�Bw�Bm�B0!B��B��B� Bs�Bm�BffBZBJ�B=qB6FB/B$�B�B��B�B�fB��BɺBǮB�XB�!B��B��B�uB�Bo�B^5BYBW
BVBS�BN�B=qB�BDB
��B
�B
�B
�NB
��B
ƨB
�3B
�!B
�B
��B
��B
�hB
�=B
�B
|�B
y�B
w�B
s�B
e`B
\)B
XB
S�B
N�B
E�B
@�B
;dB
6FB
.B
)�B
�B
PB
bB
+B	��B	��B	�B	�yB	�mB	�fB	�TB	�5B	��B	ŢB	�?B	�'B	�B	�B	��B	�B	�'B	�-B	�B	��B	��B	��B	��B	�bB	�7B	�B	� B	v�B	k�B	bNB	]/B	W
B	N�B	E�B	8RB	.B	#�B	�B	uB	bB	PB	JB	VB	oB	PB	B��B�B�mB�ZB�/B�#B�B��B��B��B��BȴBŢBÖBB��B�}B�wB�LB�3B�3B�3B�!B�B�B�B��B��B��B��B��B��B��B��B�uB�bB�JB�=B�B�B~�B}�B|�B{�B{�By�Bw�Bu�Bs�Bp�Bn�Bm�BjBgmBffBdZB`BB]/B[#BVBR�BP�BO�BM�BL�BK�BH�BG�BF�BC�B@�B=qB:^B9XB7LB5?B33B/B/B.B-B-B+B(�B'�B%�B%�B!�B!�B �B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B{BuBuBuBuBoBoB{BoBoBhBhBbB{BuBuBuBoBoBoBhBoBhB{B�B�B{B�B�B�B�B�B�B�B�B�B�B�B�B�B!�B$�B#�B#�B(�B,B-B/B2-B33B49B5?B6FB7LB8RB9XB:^B=qBE�BH�BK�BM�BO�BP�BQ�BQ�BQ�BQ�BS�BS�BT�BVBW
BW
BYB_;BcTBe`BffBgmBiyBk�Bm�Bn�Bo�Bw�B{�B|�B~�B�B�B�%B�7B�JB�VB�\B�oB�uB�uB��B��B��B��B��B��B��B��B��B�B�!B�!B�'B�-B�9B�XB�qB��B��BBǮBȴB��B��B��B�B�B�B�#B�;B�HB�TB�ZB�fB�B�B�B�B��B��B��B	B	%B		7B		7B	DB	bB	uB	�B	�B	�B	�B	�B	�B	�B	�B	�B	!�B	#�B	$�B	)�B	,B	1'B	33B	49B	7LB	9XB	<jB	=qB	A�B	C�B	C�B	D�B	G�B	I�B	M�B	O�B	P�B	R�B	VB	W
B	XB	YB	[#B	^5B	aHB	cTB	e`B	ffB	gmB	hsB	iyB	k�B	m�B	o�B	r�B	t�B	v�B	x�B	{�B	|�B	}�B	� B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�%B	�DB	�VB	�\B	�bB	�hB	�oB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�'B	�3B	�?B	�FB	�LB	�RB	�XB	�XB	�XB	�^B	�jB	�jB	�wB	�}B	B	ÖB	ÖB	ĜB	ĜB	ƨB	ǮB	ǮB	ȴB	ȴB	ɺB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�B	�B	�#B	�)B	�)B	�/B	�/B	�/B	�5B	�;B	�;B	�HB	�NB	�TB	�ZB	�`B	�`B	�fB	�fB	�fB	�fB	�yB	�yB	�yB	�yB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
  B
  B
  B
B
B
B
B
B
B
B
B
B
B
%B
%B
+B
1B
	7B
	7B
	7B
	7B

=B
DB
JB
PB
PB
VB
\B
bB
bB
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
{B
{B
{B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
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
!�B
"�B
"�B
"�B
#�B
#�B
$�B
%�B
%�B
%�B
&�B
&�B
&�B
&�B
'�B
'�B
(�B
(�B
(�B
)�B
+B
+B
+B
+B
,B
-B
-B
-B
.B
.B
.B
.B
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
1'B
1'B
2-B
33B
33B
33B
49B
49B
49B
5?B
6FB
6FB
6FB
6FB
7LB
7LB
7LB
7LB
8RB
9XB
9XB
9XB
9XB
:^B
:^B
:^B
:^B
:^B
:^B
:^B
:^B
:^B
:^B
:^B
;dB
<jB
=qB
=qB
=qB
=qB
>wB
>wB
>wB
>wB
?}B
@�B
@�B
@�B
@�B
A�B
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
E�B
E�B
F�B
F�B
F�B
F�B
F�B
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
I�B
I�B
I�B
I�B
I�B
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
M�B
M�B
M�B
M�B
M�B
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
O�B
O�B
O�B
P�B
P�B
P�B
R�B
R�B
R�B
R�B
R�B
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
ZB
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
ffB
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
l�B
l�B
l�B
m�B
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
q�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�PRES            TEMP            PSAL            PRES_ADJUSTED = PRES - surface_pressure                                                                                                                                                                                                                         none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            surface_pressure=-0.10 dbar                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                     Pressure adjusted at real time based on most recent valid surface pressure                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                      20221115110152                              AO  ARCAADJP                                                                    20221115110152    IP                G�O�G�O�G�O�                AO  ARGQQCPL                                                                    20221115110152  QCP$                G�O�G�O�G�O�1F83E           AO  ARGQQCPL                                                                    20221115110152  QCF$                G�O�G�O�G�O�4000            