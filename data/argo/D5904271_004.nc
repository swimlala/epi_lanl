CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  e   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       2014-07-22T01:11:09Z creation      
references        (http://www.argodatamgt.org/Documentation   comment           user_manual_version       3.1    Conventions       Argo-3.1 CF-1.6    featureType       trajectoryProfile         @   	DATA_TYPE                  	long_name         	Data type      
_FillValue               conventions       Argo reference table 1          6�   FORMAT_VERSION                 	long_name         File format version    
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
_FillValue                    7�   PLATFORM_TYPE                     	long_name         Type of float      
_FillValue               conventions       Argo reference table 23          7�   WMO_INST_TYPE                     	long_name         Coded instrument type      conventions       Argo reference table 8     
_FillValue                    7�   JULD               	long_name         ?Julian day (UTC) of the station relative to REFERENCE_DATE_TIME    standard_name         time   units         "days since 1950-01-01 00:00:00 UTC     conventions       8Relative julian days with decimal part (as parts of day)   
_FillValue        A.�~       axis      T      
resolution        >�E�vQ�        7�   JULD_QC                	long_name         Quality on date and time   conventions       Argo reference table 2     
_FillValue                    7�   JULD_LOCATION                  	long_name         @Julian day (UTC) of the location relative to REFERENCE_DATE_TIME   units         "days since 1950-01-01 00:00:00 UTC     conventions       8Relative julian days with decimal part (as parts of day)   
_FillValue        A.�~       
resolution        >�E�vQ�        7�   LATITUDE               	long_name         &Latitude of the station, best estimate     standard_name         latitude   units         degree_north   
_FillValue        @�i�       	valid_min         �V�        	valid_max         @V�        axis      Y           8   	LONGITUDE                  	long_name         'Longitude of the station, best estimate    standard_name         	longitude      units         degree_east    
_FillValue        @�i�       	valid_min         �f�        	valid_max         @f�        axis      X           8   POSITION_QC                	long_name         ,Quality on position (latitude and longitude)   conventions       Argo reference table 2     
_FillValue                    8   POSITIONING_SYSTEM                    	long_name         Positioning system     
_FillValue                    8   VERTICAL_SAMPLING_SCHEME                  	long_name         Vertical sampling scheme   conventions       Argo reference table 16    
_FillValue                    8    CONFIG_MISSION_NUMBER                  	long_name         :Unique number denoting the missions performed by the float     conventions       !1...N, 1 : first complete mission      
_FillValue         ��        9    PROFILE_PRES_QC                	long_name         #Global quality flag of PRES profile    conventions       Argo reference table 2a    
_FillValue                    9$   PROFILE_TEMP_QC                	long_name         #Global quality flag of TEMP profile    conventions       Argo reference table 2a    
_FillValue                    9(   PROFILE_PSAL_QC                	long_name         #Global quality flag of PSAL profile    conventions       Argo reference table 2a    
_FillValue                    9,   PRES         
      
   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���   axis      Z        �  90   PRES_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 h  F�   PRES_ADJUSTED            
      	   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  J,   PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 h  W�   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         decibar    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  [(   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  h�   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 h  vP   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  y�   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 h  �L   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         degree_Celsius     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  ��   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  �H   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 h  ��   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  �D   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 h  ��   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  �@   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  0  ��   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                    �   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                    �   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                    �   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    
_FillValue               conventions       YYYYMMDDHHMISS        ,  �   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                    �p   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                    �t   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                    �x   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                    �|   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  @  р   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                    ��   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                    ��   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    ��   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar         ��   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar         ��   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�        ��   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    ��   FLOAT_SERIAL_NO                   	long_name         Serial number of the float     
_FillValue                     �0   FIRMWARE_VERSION                  	long_name         Instrument firmware version    
_FillValue                     �P         �PArgo profile    3.1 1.2 19500101000000  20140722011109  20181023142215  5904271 US ARGO PROJECT                                                 GREGORY C. JOHNSON                                              PRES            TEMP            PSAL               A   AO  4744_0188_004                   2C  D   NAVIS_A                         863 @֛Y�  1   @֛[  @*�t�j~��c��t�1   GPS     Primary sampling: averaged [1Hz sampling by SBE-41CP averaged in 2-dbar bins]                                                                                                                                                                                      A   A   A   @���@�  A   A   A@  A`  A�  A�  A�  A�  A�  A�  A�  A�  B   B  B  B  B   B(  B0  B8  B@  BH  BP  BX  B`  Bh  Bp  Bx  B�33B�  B���B�  B�  B�  B�  B�  B�  B�  B�33B�  B�  B���B���B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  C   C  C  C  C  C
  C  C  C  C  C  C  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dt� Du  Du� Dv  Dv� Dw  Dw� Dx  Dx� Dy  Dy� Dz  Dz� D{  D{� D|  D|� D}  D}� D~  D~� D  D� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D D�� D�  D�@ DÀ D�� D�  D�@ DĀ D�� D�  D�@ Dŀ D�� D�  D�@ Dƀ D�� D�  D�@ Dǀ D�� D�  D�@ DȀ D�� D�  D�@ Dɀ D�� D�  D�@ Dʀ D�� D�  D�@ Dˀ D�� D�  D�@ D̀ D�� D�  D�@ D̀ D�� D�  D�@ D΀ D�� D�  D�@ Dπ D�� D�  D�@ DЀ D�� D�  D�@ Dр D�� D�  D�@ DҀ D�� D�  D�@ DӀ D�� D�  D�@ DԀ D�� D�  D�@ DՀ D�� D�  D�@ Dր D�� D�  D�@ D׀ D�� D�  D�@ D؀ D�� D�3D�FfD�` 11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   @���@�33@�33A��A=��A]��A}��A���A���A���A���A���A���A���A���BffBffBffBffB'ffB/ffB7ffB?ffBGffBOffBWffB_ffBgffBoffBwffB��B��3B�� B��3B��3B��3B��3B��3B��3B��3B��fB��3B��3B�� B�� B��3B��3Bó3Bǳ3B˳3Bϳ3Bӳ3B׳3B۳3B߳3B�3B�3B�3B�3B�3B��3B��3B��3CٚCٚCٚCٚC	ٚCٚCٚCٚCٚCٚCٚCٚCٚCٚCٚCٚC!ٚC#ٚC%ٚC'ٚC)ٚC+ٚC-ٚC/ٚC1ٚC3ٚC5ٚC7ٚC9ٚC;ٚC=ٚC?ٚCAٚCCٚCEٚCGٚCIٚCKٚCMٚCOٚCQٚCSٚCUٚCWٚCYٚC[ٚC]ٚC_ٚCaٚCcٚCeٚCgٚCiٚCkٚCmٚCoٚCqٚCsٚCuٚCwٚCyٚC{ٚC}ٚCٚC���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���D vfD �fDvfD�fDvfD�fDvfD�fDvfD�fDvfD�fDvfD�fDvfD�fDvfD�fD	vfD	�fD
vfD
�fDvfD�fDvfD�fDvfD�fDvfD�fDvfD�fDvfD�fDvfD�fDvfD�fDvfD�fDvfD�fDvfD�fDvfD�fDvfD�fDvfD�fDvfD�fDvfD�fDvfD�fDvfD�fDvfD�fDvfD�fDvfD�fD vfD �fD!vfD!�fD"vfD"�fD#vfD#�fD$vfD$�fD%vfD%�fD&vfD&�fD'vfD'�fD(vfD(�fD)vfD)�fD*vfD*�fD+vfD+�fD,vfD,�fD-vfD-�fD.vfD.�fD/vfD/�fD0vfD0�fD1vfD1�fD2vfD2�fD3vfD3�fD4vfD4�fD5vfD5�fD6vfD6�fD7vfD7�fD8vfD8�fD9vfD9�fD:vfD:�fD;vfD;�fD<vfD<�fD=vfD=�fD>vfD>�fD?vfD?�fD@vfD@�fDAvfDA�fDBvfDB�fDCvfDC�fDDvfDD�fDEvfDE�fDFvfDF�fDGvfDG�fDHvfDH�fDIvfDI�fDJvfDJ�fDKvfDK�fDLvfDL�fDMvfDM�fDNvfDN�fDOvfDO�fDPvfDP�fDQvfDQ�fDRvfDR�fDSvfDS�fDTvfDT�fDUvfDU�fDVvfDV�fDWvfDW�fDXvfDX�fDYvfDY�fDZvfDZ�fD[vfD[�fD\vfD\�fD]vfD]�fD^vfD^�fD_vfD_�fD`vfD`�fDavfDa�fDbvfDb�fDcvfDc�fDdvfDd�fDevfDe�fDfvfDf�fDgvfDg�fDhvfDh�fDivfDi�fDjvfDj�fDkvfDk�fDlvfDl�fDmvfDm�fDnvfDn�fDovfDo�fDpvfDp�fDqvfDq�fDrvfDr�fDsvfDs�fDtvfDt�fDuvfDu�fDvvfDv�fDwvfDw�fDxvfDx�fDyvfDy�fDzvfDz�fD{vfD{�fD|vfD|�fD}vfD}�fD~vfD~�fDvfD�fD�;3D�{3D��3D��3D�;3D�{3D��3D��3D�;3D�{3D��3D��3D�;3D�{3D��3D��3D�;3D�{3D��3D��3D�;3D�{3D��3D��3D�;3D�{3D��3D��3D�;3D�{3D��3D��3D�;3D�{3D��3D��3D�;3D�{3D��3D��3D�;3D�{3D��3D��3D�;3D�{3D��3D��3D�;3D�{3D��3D��3D�;3D�{3D��3D��3D�;3D�{3D��3D��3D�;3D�{3D��3D��3D�;3D�{3D��3D��3D�;3D�{3D��3D��3D�;3D�{3D��3D��3D�;3D�{3D��3D��3D�;3D�{3D��3D��3D�;3D�{3D��3D��3D�;3D�{3D��3D��3D�;3D�{3D��3D��3D�;3D�{3D��3D��3D�;3D�{3D��3D��3D�;3D�{3D��3D��3D�;3D�{3D��3D��3D�;3D�{3D��3D��3D�;3D�{3D��3D��3D�;3D�{3D��3D��3D�;3D�{3D��3D��3D�;3D�{3D��3D��3D�;3D�{3D��3D��3D�;3D�{3D��3D��3D�;3D�{3D��3D��3D�;3D�{3D��3D��3D�;3D�{3D��3D��3D�;3D�{3D��3D��3D�;3D�{3D��3D��3D�;3D�{3D��3D��3D�;3D�{3D��3D��3D�;3D�{3D��3D��3D�;3D�{3D��3D��3D�;3D�{3D��3D��3D�;3D�{3D��3D��3D�;3D�{3D��3D��3D�;3D�{3D��3D��3D�;3D�{3D��3D��3D�;3D�{3D��3D��3D�;3D�{3D��3D��3D�;3D�{3D��3D��3D�;3D�{3D��3D��3D�;3D�{3D��3D��3D�;3D�{3D��3D��3D�;3D�{3D��3D��3D�;3D�{3D��3D��3D�;3D�{3D��3D��3D�;3D�{3D��3D��3D�;3D�{3D��3D��3D�;3D�{3D��3D��3D�;3D�{3D��3D��3D�;3D�{3D��3D��3D�;3D�{3D��3D��3D�;3D�{3D��3D��3D�;3D�{3D��3D��3D�;3D�{3D»3D��3D�;3D�{3Dû3D��3D�;3D�{3DĻ3D��3D�;3D�{3DŻ3D��3D�;3D�{3Dƻ3D��3D�;3D�{3Dǻ3D��3D�;3D�{3DȻ3D��3D�;3D�{3Dɻ3D��3D�;3D�{3Dʻ3D��3D�;3D�{3D˻3D��3D�;3D�{3D̻3D��3D�;3D�{3Dͻ3D��3D�;3D�{3Dλ3D��3D�;3D�{3Dϻ3D��3D�;3D�{3Dл3D��3D�;3D�{3Dѻ3D��3D�;3D�{3Dһ3D��3D�;3D�{3Dӻ3D��3D�;3D�{3DԻ3D��3D�;3D�{3Dջ3D��3D�;3D�{3Dֻ3D��3D�;3D�{3D׻3D��3D�;3D�{3Dػ3D��fD�A�D�[311111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   @��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��A�I�A�Q�A�E�A�O�A�^5A�`BA�`BA�bNA�\)A�^5A�`BA�`BA�`BA�dZA�jA�jA�n�A�n�A�p�A�p�A�r�A�r�A�r�A�r�A�r�A�r�A�t�A�v�A�r�A�r�A�S�A��A�t�AΥ�Aʺ^A��AǋDAƸRAŃAĸRA��A�+A��TA���A���A���A�VA�r�A��+A��HA�A�A��mA��HA���A�
=A���A�9XA��A��A��+A��A���A�VA��A��-A�-A�E�A���A��A�ƨA�{A�1A�(�A�x�A�\)A��wA�Q�A�oA���A��#A~�AyhsAt��Ap�9Alr�AfQ�A^(�AY&�AS%AP�AO��AL�RAH��AGt�AC�AAA>�uA=\)A<�uA;&�A:(�A9?}A7;dA6 �A4�A2�`A29XA1�A0�A/?}A/C�A.��A,�yA*��A*��A*bA(�`A%|�A#&�A$�A$��A"��A!��A�HA33A��A�\A{A9XA��A��A�^A�
A��A�TA�Av�A1'A��A�/AA�\AM�AK�Al�A=qA��A|�A`BAG�A~�A�wA�A1A��AK�A�AZAx�A�A�/A5?A�#A�FAdZA+A��AE�Ax�A��A9XAƨA|�A�A
��A
9XA	��A�A�A�A�
A�A"�A�
A�hAl�A�A��A��A�Av�AA��AK�A�yAE�A�;A��AhsA/A%A �`AoA ��A/A�A ��A z�A 1'@���A b@�t�@���@��T@�G�@�A�@�1'@�(�@���@��@��+@�O�@��@��@�(�@�dZ@�\)@��\@��@���@�-@�O�@���@��@��@���@�p�@�h@�-@�V@�@�1'@�S�@�ȴ@���@�Z@��;@���@�l�@�33@���@�E�@�M�@�~�@��#@�-@��@�p�@�7L@���@��`@���@蛦@�Z@��
@�C�@�"�@�ȴ@旍@�E�@�J@��@�j@��;@�@�o@�ff@�@ᙚ@��@އ+@�X@�?}@�O�@�hs@݉7@���@ܣ�@ܓu@܃@�9X@��@�\)@ڰ!@��@ٙ�@�`B@�G�@���@��@�t�@�S�@�C�@�"�@��H@֏\@�v�@�$�@Չ7@�j@Ӿw@�C�@��y@ҸR@҇+@�$�@��T@Ѳ-@�hs@�7L@�V@���@���@��/@У�@Ѓ@�9X@ϕ�@�@��@�hs@�&�@���@�j@�1@�33@�ff@�J@�`B@ȋD@�(�@Ǿw@�"�@Ɨ�@Ɨ�@�v�@�^5@���@�x�@ļj@�Q�@��@Õ�@��@��H@¸R@�~�@�n�@��@���@��@�9X@��w@�+@���@��@���@�?}@��`@��u@�ƨ@�dZ@�
=@�$�@�p�@�/@��/@�(�@���@�S�@��@��y@�~�@�-@�{@���@��/@��u@�Q�@���@�l�@�S�@���@�=q@���@�hs@��@�Q�@��@���@�"�@���@�~�@�5?@���@�`B@�V@���@��@���@�|�@���@�V@�5?@�$�@�{@��@��^@�/@�9X@�\)@�"�@�o@�
=@���@�E�@�-@���@��h@�O�@���@��9@��u@�I�@�ƨ@���@���@�|�@�dZ@�C�@�33@��y@�~�@�M�@�-@��@��h@�X@�/@���@�Ĝ@��u@�  @�dZ@�;d@�"�@�o@��H@��\@�E�@�@���@��@�p�@�X@�/@��@�Ĝ@��@���@��P@�dZ@�"�@���@��@�-@�O�@�I�@�b@��
@�l�@�+@��y@��!@���@�V@��@��#@��#@���@���@�?}@��@��@��@�j@�Z@�(�@��m@���@��@�dZ@�C�@�33@��@�@��+@���@�p�@�Ĝ@�bN@�1@��w@�|�@�\)@�33@�
=@��y@��!@�v�@�J@���@��7@�x�@�X@�G�@�/@��`@���@�bN@��;@�|�@�+@��@���@�E�@��^@��h@�hs@�X@�7L@��@��@���@�A�@�  @�|�@�;d@�
=@��@���@�V@�=q@�5?@��@��@��T@��-@��@�p�@�G�@�%@��@��9@��D@��@��@�z�@�bN@�I�@�1'@��@�t�@��@��H@�ff@��@��T@��-@��@�X@�/@��@���@��/@���@��@�z�@�9X@�  @~�+@}@}p�@|��@|z�@|�@{�F@z��@z-@zJ@y�#@y7L@xr�@w�@w��@w��@w;d@v��@u�@u��@u@u��@u�@t(�@s�
@st�@r�!@r-@q��@q&�@p��@p�u@pbN@pb@o+@n�@nv�@m�h@m`B@m/@l�@l�D@l9X@k�m@k��@kC�@j�@j��@j=q@i�#@i�7@h��@g��@g�P@g
=@fv�@e��@e�@c��@cdZ@c"�@b��@b-@a�#@a�^@ax�@aX@aG�@a�@`�`@`��@`A�@_�w@_l�@^�@]�@]��@]p�@]V@\z�@\(�@[�
@[ƨ@[�F@[��@[�@[dZ@Z��@Z-@Y��@Y��@Y�#@Y�7@Y7L@X�u@W�@W�P@W�@V�y@V�R@V��@V5?@U�-@UO�@UV@T��@T9X@T�@S�m@Sƨ@SdZ@R�@R��@R-@Q��@QX@Q7L@P�`@PbN@Ol�@N�@N�+@N5?@M�T@M�@MO�@M�@L�D@K�m@K�@K33@J�!@J�\@JJ@I��@IX@I�@H��@H1'@Gl�@G+@F�y@Fv�@F{@E��@E��@E�h@EO�@D�@Cƨ@C�@C@B��@BM�@B-@BJ@BJ@A�#@A��@Ahs@A7L@A%@@Ĝ@@��@@�@?�;@?
=@>��@>ȴ@>V@=�@=�@=�@<��@<�D@<I�@;�
@;�@;"�@:�@:��@:M�@:=q@:�@:J@9��@9�@8�`@8�`@8�9@8��@8�@8Q�@7�P@7;d@6��@6�R@6v�@6V@65?@5�T@5O�@4z�@3�
@3�@3"�@2��@2~�@2-@1�@1��@1G�@1&�@1%@0��@0�9@0�@0A�@0  @/�P@/K�@.�y@.$�@-�@-��@-@-�@-V@,�j@,Z@,�@+�@*�H@*�\@*n�@*=q@*J@)�#@)G�@)&�@)%@(Ĝ@(bN@(Q�@( �@'��@'+@'�@&��@&�y@&�@&E�@%�-@%`B@%�@$�j@$�D@$z�@$j@$Z@$9X@$1@#�F@#��@#�@#dZ@#"�@"��@"M�@!��@!��@!7L@!%@ �9@ 1'@�@�w@��@�P11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   A�I�A�Q�A�E�A�O�A�^5A�`BA�`BA�bNA�\)A�^5A�`BA�`BA�`BA�dZA�jA�jA�n�A�n�A�p�A�p�A�r�A�r�A�r�A�r�A�r�A�r�A�t�A�v�A�r�A�r�A�S�A��A�t�AΥ�Aʺ^A��AǋDAƸRAŃAĸRA��A�+A��TA���A���A���A�VA�r�A��+A��HA�A�A��mA��HA���A�
=A���A�9XA��A��A��+A��A���A�VA��A��-A�-A�E�A���A��A�ƨA�{A�1A�(�A�x�A�\)A��wA�Q�A�oA���A��#A~�AyhsAt��Ap�9Alr�AfQ�A^(�AY&�AS%AP�AO��AL�RAH��AGt�AC�AAA>�uA=\)A<�uA;&�A:(�A9?}A7;dA6 �A4�A2�`A29XA1�A0�A/?}A/C�A.��A,�yA*��A*��A*bA(�`A%|�A#&�A$�A$��A"��A!��A�HA33A��A�\A{A9XA��A��A�^A�
A��A�TA�Av�A1'A��A�/AA�\AM�AK�Al�A=qA��A|�A`BAG�A~�A�wA�A1A��AK�A�AZAx�A�A�/A5?A�#A�FAdZA+A��AE�Ax�A��A9XAƨA|�A�A
��A
9XA	��A�A�A�A�
A�A"�A�
A�hAl�A�A��A��A�Av�AA��AK�A�yAE�A�;A��AhsA/A%A �`AoA ��A/A�A ��A z�A 1'@���A b@�t�@���@��T@�G�@�A�@�1'@�(�@���@��@��+@�O�@��@��@�(�@�dZ@�\)@��\@��@���@�-@�O�@���@��@��@���@�p�@�h@�-@�V@�@�1'@�S�@�ȴ@���@�Z@��;@���@�l�@�33@���@�E�@�M�@�~�@��#@�-@��@�p�@�7L@���@��`@���@蛦@�Z@��
@�C�@�"�@�ȴ@旍@�E�@�J@��@�j@��;@�@�o@�ff@�@ᙚ@��@އ+@�X@�?}@�O�@�hs@݉7@���@ܣ�@ܓu@܃@�9X@��@�\)@ڰ!@��@ٙ�@�`B@�G�@���@��@�t�@�S�@�C�@�"�@��H@֏\@�v�@�$�@Չ7@�j@Ӿw@�C�@��y@ҸR@҇+@�$�@��T@Ѳ-@�hs@�7L@�V@���@���@��/@У�@Ѓ@�9X@ϕ�@�@��@�hs@�&�@���@�j@�1@�33@�ff@�J@�`B@ȋD@�(�@Ǿw@�"�@Ɨ�@Ɨ�@�v�@�^5@���@�x�@ļj@�Q�@��@Õ�@��@��H@¸R@�~�@�n�@��@���@��@�9X@��w@�+@���@��@���@�?}@��`@��u@�ƨ@�dZ@�
=@�$�@�p�@�/@��/@�(�@���@�S�@��@��y@�~�@�-@�{@���@��/@��u@�Q�@���@�l�@�S�@���@�=q@���@�hs@��@�Q�@��@���@�"�@���@�~�@�5?@���@�`B@�V@���@��@���@�|�@���@�V@�5?@�$�@�{@��@��^@�/@�9X@�\)@�"�@�o@�
=@���@�E�@�-@���@��h@�O�@���@��9@��u@�I�@�ƨ@���@���@�|�@�dZ@�C�@�33@��y@�~�@�M�@�-@��@��h@�X@�/@���@�Ĝ@��u@�  @�dZ@�;d@�"�@�o@��H@��\@�E�@�@���@��@�p�@�X@�/@��@�Ĝ@��@���@��P@�dZ@�"�@���@��@�-@�O�@�I�@�b@��
@�l�@�+@��y@��!@���@�V@��@��#@��#@���@���@�?}@��@��@��@�j@�Z@�(�@��m@���@��@�dZ@�C�@�33@��@�@��+@���@�p�@�Ĝ@�bN@�1@��w@�|�@�\)@�33@�
=@��y@��!@�v�@�J@���@��7@�x�@�X@�G�@�/@��`@���@�bN@��;@�|�@�+@��@���@�E�@��^@��h@�hs@�X@�7L@��@��@���@�A�@�  @�|�@�;d@�
=@��@���@�V@�=q@�5?@��@��@��T@��-@��@�p�@�G�@�%@��@��9@��D@��@��@�z�@�bN@�I�@�1'@��@�t�@��@��H@�ff@��@��T@��-@��@�X@�/@��@���@��/@���@��@�z�@�9X@�  @~�+@}@}p�@|��@|z�@|�@{�F@z��@z-@zJ@y�#@y7L@xr�@w�@w��@w��@w;d@v��@u�@u��@u@u��@u�@t(�@s�
@st�@r�!@r-@q��@q&�@p��@p�u@pbN@pb@o+@n�@nv�@m�h@m`B@m/@l�@l�D@l9X@k�m@k��@kC�@j�@j��@j=q@i�#@i�7@h��@g��@g�P@g
=@fv�@e��@e�@c��@cdZ@c"�@b��@b-@a�#@a�^@ax�@aX@aG�@a�@`�`@`��@`A�@_�w@_l�@^�@]�@]��@]p�@]V@\z�@\(�@[�
@[ƨ@[�F@[��@[�@[dZ@Z��@Z-@Y��@Y��@Y�#@Y�7@Y7L@X�u@W�@W�P@W�@V�y@V�R@V��@V5?@U�-@UO�@UV@T��@T9X@T�@S�m@Sƨ@SdZ@R�@R��@R-@Q��@QX@Q7L@P�`@PbN@Ol�@N�@N�+@N5?@M�T@M�@MO�@M�@L�D@K�m@K�@K33@J�!@J�\@JJ@I��@IX@I�@H��@H1'@Gl�@G+@F�y@Fv�@F{@E��@E��@E�h@EO�@D�@Cƨ@C�@C@B��@BM�@B-@BJ@BJ@A�#@A��@Ahs@A7L@A%@@Ĝ@@��@@�@?�;@?
=@>��@>ȴ@>V@=�@=�@=�@<��@<�D@<I�@;�
@;�@;"�@:�@:��@:M�@:=q@:�@:J@9��@9�@8�`@8�`@8�9@8��@8�@8Q�@7�P@7;d@6��@6�R@6v�@6V@65?@5�T@5O�@4z�@3�
@3�@3"�@2��@2~�@2-@1�@1��@1G�@1&�@1%@0��@0�9@0�@0A�@0  @/�P@/K�@.�y@.$�@-�@-��@-@-�@-V@,�j@,Z@,�@+�@*�H@*�\@*n�@*=q@*J@)�#@)G�@)&�@)%@(Ĝ@(bN@(Q�@( �@'��@'+@'�@&��@&�y@&�@&E�@%�-@%`B@%�@$�j@$�D@$z�@$j@$Z@$9X@$1@#�F@#��@#�@#dZ@#"�@"��@"M�@!��@!��@!7L@!%@ �9@ 1'@�@�w@��@�P11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   ;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBB  B
��B
�B
�B2-B�B�{B�9B�
B�ZB%B\)BgmB~�BhsB<jB49B1'B1'B�B\BoB�B0!B49B7LB5?B6FB7LB7LBC�BI�BD�B=qB:^B<jB/B$�BuB�BǮB�B�BXBPB
�B
�B
�JB
B�B
JB	�)B	�RB	��B	z�B	^5B	9XB	oB��B�B�mB�TB�fB�B�B	  B	B		7B	%B	+B	JB	hB	uB	�B	oB	oB	#�B	%�B	)�B	.B	>wB	YB	bNB	`BB	l�B	y�B	�B	~�B	cTB	VB	� B	�B	~�B	}�B	ZB	W
B	W
B	ZB	^5B	iyB	y�B	� B	��B	��B	��B	�hB	�bB	�PB	�JB	��B	�B	�3B	�'B	�'B	�B	�LB	ǮB	��B	��B	��B	��B	ȴB	ŢB	��B	�jB	�dB	�XB	�dB	�jB	�XB	�jB	�}B	�}B	��B	ÖB	ĜB	ĜB	ÖB	B	��B	�qB	�qB	�}B	��B	��B	��B	�wB	�wB	�}B	�wB	�qB	�jB	�dB	�LB	�B	�B	�B	�-B	�9B	�FB	�XB	�wB	�}B	�}B	��B	��B	��B	ÖB	ŢB	ǮB	��B	��B	�B	�5B	�BB	�sB	�yB	�sB	�sB	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
B
B
B
B
  B	��B	��B	��B
B
B
B
B
+B

=B
JB
hB
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
{B
uB
uB
oB
hB
hB
hB
hB
bB
bB
\B
\B
JB
B
B
%B
1B
	7B

=B
DB

=B
JB
VB
VB
PB
JB
JB
JB
DB
DB
DB
DB

=B
DB
DB
DB
DB
DB
DB

=B

=B
	7B
+B
B
B
B
B
B
B
B
B
B
B
B
%B
+B
	7B

=B

=B
	7B
1B
1B
	7B
1B
1B

=B

=B
1B
+B
B
B
%B
1B
+B
%B
B
%B
%B
%B
%B
%B
B
B
B
%B
%B
+B
+B
+B
+B
+B
%B
%B
%B
%B
%B
+B
+B
1B
	7B
	7B
	7B

=B
DB
DB

=B
JB
JB
JB
JB
JB
JB
PB
PB
PB
PB
PB
PB
PB
VB
VB
VB
VB
\B
VB
\B
\B
\B
\B
bB
bB
bB
hB
hB
hB
hB
hB
oB
oB
oB
oB
oB
uB
oB
uB
uB
uB
uB
uB
uB
uB
uB
{B
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
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
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
!�B
!�B
"�B
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
%�B
%�B
%�B
%�B
%�B
%�B
%�B
%�B
$�B
$�B
$�B
%�B
&�B
&�B
'�B
'�B
(�B
(�B
(�B
(�B
(�B
)�B
,B
,B
,B
,B
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
/B
/B
/B
/B
/B
/B
.B
.B
.B
0!B
1'B
2-B
2-B
33B
33B
33B
33B
49B
49B
49B
49B
49B
49B
49B
5?B
5?B
5?B
5?B
5?B
5?B
5?B
6FB
6FB
5?B
5?B
6FB
6FB
7LB
7LB
7LB
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
9XB
9XB
;dB
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
>wB
>wB
>wB
?}B
?}B
?}B
?}B
?}B
?}B
@�B
@�B
@�B
A�B
A�B
A�B
B�B
B�B
B�B
B�B
B�B
C�B
C�B
C�B
D�B
D�B
D�B
D�B
D�B
D�B
D�B
E�B
E�B
E�B
E�B
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
K�B
L�B
L�B
L�B
M�B
M�B
M�B
M�B
N�B
N�B
N�B
N�B
N�B
O�B
N�B
N�B
O�B
O�B
P�B
P�B
O�B
O�B
O�B
P�B
P�B
P�B
O�B
O�B
O�B
O�B
P�B
O�B
P�B
P�B
P�B
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
R�B
R�B
R�B
R�B
R�B
S�B
S�B
R�B
S�B
S�B
S�B
S�B
T�B
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
VB
VB
W
B
W
B
W
B
XB
YB
YB
XB
YB
XB
XB
XB
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
ZB
[#B
[#B
\)B
\)B
\)B
]/B
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
aHB
aHB
aHB
aHB
aHB
aHB
aHB
bNB
cTB
dZB
dZB
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
o�B
o�B
p�B
p�B
p�B
p�B
q�B
q�B
q�B
r�B
r�B
r�B
r�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   B�BAB�B�BBBB5BBB BBBB BBBBBBBBBB BBB$B"BzB �B
��B
�4B
�B;XB�rB�eB�lB��B�4B�B`�Bk�B��Bt�B@�B>�B8�B7�B%�BjBZB&B7�B9�B;7B6�B7�B8MB9�BF�BL�BG�BB,B=wBA�B2B(�B7B��B��B��B�Be�B	B
��B
�B
��B
RB
�B	�NB	�:B	�B	��B	g�B	F4B	�B	�B��B�BB�jB�B�B�B	^B	 B	OB	ZB	
�B	�B	�B	�B	�B	�B	�B	%�B	(�B	,'B	/�B	>�B	Y�B	e�B	c�B	l�B	{B	�B	��B	gCB	SB	�B	�[B	��B	��B	]�B	X\B	WJB	[AB	]�B	h�B	y�B	}\B	�uB	��B	�B	��B	�6B	�B	��B	��B	��B	�}B	�B	��B	��B	�TB	�nB	� B	�3B	�7B	��B	��B	��B	��B	��B	�:B	�\B	� B	��B	�eB	�(B	�EB	��B	� B	ĈB	�TB	��B	��B	��B	��B	��B	��B	�UB	��B	�cB	�EB	�B	��B	��B	��B	�AB	�B	�B	�B	��B	��B	��B	��B	�CB	��B	�GB	��B	�~B	�RB	B	�B	B	�KB	�)B	�NB	�JB	�OB	խB	ނB	��B	�B	�LB	�VB	�@B	�GB	�@B	�B	�B	�B	�B	��B	��B	��B	�|B	��B	��B	�?B	�B	�XB	�B	��B	��B	��B	��B	��B	�wB	�B	��B	�B	��B	��B	��B	��B	��B	��B	��B
�B
/B
�B
�B
�B	��B	�&B	��B
mB
yB
B
B
B
$B
�B
�B
OB
�B
�B
�B
�B
�B
 B
hB
eB
�B
B
�B
�B
�B
�B
�B
B
�B
8B
IB
�B
�B
�B
�B
�B
PB
!B
#B
	%B
B
�B

cB
uB
�B
�B
$B
@B
KB
�B
�B
|B
�B
�B
4B
�B
qB
�B
�B
�B
yB

�B
*B

�B
'B
�B
�B
hB
eB
�B
xB
eB
�B
\B
UB
HB
9B
dB
	�B

~B

�B

-B
	B
	�B
	�B
�B
�B
B

�B
	[B
SB
�B
B
fB
�B
�B
B
�B
:B
fB
^B
�B
�B
3B
�B
�B
�B
�B
�B
vB
�B
UB
�B
�B

B
sB
�B
�B
�B
)B
�B
	�B
	�B
	�B
hB
�B
�B
�B
PB
�B
�B
RB
�B
B
�B
�B
�B
�B
�B
B
mB
�B
�B
�B
*B
�B
WB
�B
�B
�B
<B
�B
�B
�B
B
�B
�B
�B
B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
<B
�B
�B
�B
�B
�B
�B
5B
�B
B
�B
�B
B
B
�B
B
[B
�B
�B
�B
�B
�B
�B
B
?B
�B
�B
B
=B
B
�B
�B
B
B
�B
�B
�B
�B
�B
	B
8B
(B
"B
@B
�B
�B
�B
B
)B
B
/B
�B
oB
B
.B
B
B
�B
 B
 @B
 B
 .B
!mB
!;B
!3B
",B
!�B
"EB
",B
"9B
"�B
!�B
"B
#bB
#B
#B
#kB
$
B
$B
$'B
%DB
%BB
&!B
&"B
& B
&B
&B
&B
&�B
&�B
%_B
%�B
%jB
&nB
'WB
'OB
(/B
(;B
)FB
)7B
)YB
)`B
)�B
*�B
,;B
,/B
,IB
,2B
,DB
,~B
,oB
-^B
-�B
-�B
.�B
.�B
/~B
/�B
/�B
/bB
/cB
/EB
/\B
/ZB
/0B
/�B
.�B
.�B
.�B
0�B
1�B
2gB
2�B
3�B
3kB
3SB
3kB
4�B
4hB
4�B
4�B
4hB
4�B
4�B
5|B
5�B
5�B
5^B
5UB
5aB
5zB
6|B
6�B
5�B
6B
6�B
6�B
8B
7�B
7�B
8�B
8�B
8�B
8�B
9�B
9�B
9�B
9~B
9�B
9�B
9�B
9�B
:~B
<B
;�B
;�B
;�B
<�B
<�B
=0B
=�B
=�B
=�B
>B
>B
>�B
>�B
>�B
>�B
?B
@B
?�B
?�B
?�B
?�B
@CB
@�B
@�B
A$B
A�B
A�B
A�B
B�B
B�B
B�B
B�B
C9B
C�B
C�B
DEB
D�B
D�B
D�B
D�B
D�B
D�B
D�B
E�B
E�B
E�B
E�B
F�B
F�B
GaB
GSB
G�B
HB
I2B
I]B
I+B
I�B
J<B
KB
K3B
K)B
LB
K�B
LB
K�B
K�B
LB
K�B
L
B
L$B
L=B
M!B
MQB
M�B
N%B
NB
N5B
NVB
O+B
O$B
N�B
N�B
OB
O�B
OB
OUB
PHB
PB
P�B
QB
P%B
P*B
PYB
QcB
Q6B
QAB
PB
PB
PB
P3B
QOB
P5B
Q#B
Q'B
Q]B
QB
QB
QB
Q>B
QHB
Q3B
QFB
RCB
RMB
RB
R9B
RbB
R�B
SnB
S?B
SCB
SDB
SMB
T1B
T4B
SpB
T�B
TVB
TIB
TiB
U+B
TmB
UUB
UNB
UAB
UeB
UgB
U�B
VJB
VHB
VpB
VdB
VnB
WB
W-B
WYB
Y
B
YhB
Y`B
X�B
YcB
XyB
X=B
X<B
Y.B
YTB
ZeB
ZVB
ZYB
ZYB
ZcB
ZOB
ZSB
Z�B
Z�B
[BB
[dB
\�B
\�B
\�B
]�B
]�B
]yB
]tB
]�B
]~B
^�B
^iB
^eB
^�B
_YB
_aB
_XB
_xB
_�B
`vB
`VB
`pB
`^B
`lB
`uB
`�B
`�B
a�B
a�B
a�B
arB
asB
a�B
a�B
b�B
c�B
d�B
d�B
d�B
d�B
e�B
e�B
e�B
e�B
f�B
f�B
f�B
f�B
f�B
f�B
g�B
g�B
g�B
g�B
hB
h�B
h�B
h�B
h�B
h�B
h�B
h�B
i�B
i�B
j B
j�B
j�B
j�B
j�B
j�B
kB
j�B
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
mB
nB
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
pB
p	B
p�B
qB
qB
p�B
r B
rB
q�B
r�B
r�B
r�B
r�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   <#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<<P<(!<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<h=�<S�<<#�
<9�<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<l��<#�
<#�
<<�zE<��/<x��<D:�<#�
<#�
<#�
</v<]ul<&�H<Mj�<#�
<#�
<#�
<'i�<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
</&�<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
PRES            TEMP            PSAL            PRES_ADJUSTED = PRES - (REPORTED_SURFACE_PRESSURE) for PADJ                                                                                                                                                                                                     none                                                                                                                                                                                                                                                            PSAL_ADJ corrects Cnd. Thermal Mass (CTM), Johnson et al,2007,JAOT & effects of pressure adjustments                                                                                                                                                            PADJ REPORTED_SURFACE_PRESSURE =0.15 dbar                                                                                                                                                                                                                       none                                                                                                                                                                                                                                                            CTM alpha = 0.141 & tau = 6.68 s with error equal to the correction                                                                                                                                                                                             Pressures adjusted using reported surface pressure when warranted.                                                                                                                                                                                              none                                                                                                                                                                                                                                                            Salinity corrected for pressure sensor calibration drift and cell thermal mass                                                                                                                                                                                  201810180917432018101809174320181018091743  0188                            052512                          AO  ARCAADJP                                                                    20140722011109    IP                G�O�G�O�G�O�                AO  ARGQQCPL                                                                    20140722011109  QCP$                G�O�G�O�G�O�DFB7E           AO  ARGQQCPL                                                                    20140722011109  QCF$                G�O�G�O�G�O�0                                                                                                                                   G�O�G�O�G�O�                PM  ARSQPADJV1.1                                                                20181018091743  QC  PRES            @���D�` G�O�                PM  ARSQCTM V1.1                                                                20181018091743  QC  PSAL            @���D�` G�O�                PM  ARSQOWGUV1.0                                                                20181023142215  IP                  G�O�G�O�G�O�                