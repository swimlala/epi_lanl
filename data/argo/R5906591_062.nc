CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       2023-01-20T11:01:48Z creation      
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
_FillValue                 �  I$   PRES_ADJUSTED            
      	   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  M   PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  \�   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         decibar    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  `�   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  pl   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �    TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  �   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ��   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         degree_Celsius     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  ��   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  �h   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  �   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ��   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  ΰ   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  0  �d   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                    ޔ   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                    �   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                    �   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  ,  �   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                    ��   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                    ��   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                    ��   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                    ��   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  @  ��   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                    �   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                    �    HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    �$   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar         �4   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar         �8   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�        �<   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    �@Argo profile    3.1 1.2 19500101000000  20230120110148  20230120110148  5906591 Argo PMEL                                                       GREGORY C. JOHNSON                                              PRES            TEMP            PSAL               >A   AO  8753                            2B  A   NAVIS_A                         1281                            170425                          863 @��Ot|T1   @���s�b@1dZ��d��Q�1   GPS     Primary sampling: averaged                                                                                                                                                                                                                                         >A   A   A   @�33@�  A   A   A@  A`  A�  A�  A�  A�  A�  A�  A�  A�  B   B  B  B  B   B(  B0  B8  B@  BH  BP  BX  B`  Bh  Bp  Bx  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  C   C  C  C  C  C
  C  C  C  C  C  C�C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.�C0  C2  C4  C6  C8  C9�fC;�fC=�fC@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C]�fC`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D"��D#y�D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=fD=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dt� Du  Du� Dv  Dv� Dw  Dw� Dx  Dx� Dy  Dy� Dz  Dz� D{  D{� D|  D|� D}  D}� D~  D~� D  D� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D���D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D���D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D D�� D�  D�@ DÀ D�� D�  D�@ DĀ D�� D�  D�@ Dŀ D�� D�  D�@ Dƀ D�� D�  D�@ Dǀ D�� D�  D�@ DȀ D�� D�  D�@ Dɀ D�� D�  D�@ Dʀ D�� D�3D�C3Dˀ D�� D�  D�@ D̀ D�� D�  D�@ D̀ D�� D�  D�@ D΀ D�� D�  D�@ Dπ D�� D�  D�@ DЀ D�� D�  D�@ Dр D�� D�  D�@ DҀ D�� D�  D�@ DӀ D�� D�  D�@ DԀ D�� D�  D�@ DՀ D�� D�  D�@ Dր D�� D�  D�@ D׀ D�� D�  D�@ D؀ D�� D�  D�@ Dـ D�� D�  D�@ Dڀ D�� D�  D�@ Dۀ D�� D�  D�@ D܀ D�� D�  D�@ D݀ D�� D�  D�@ Dހ D�� D�  D�@ D߀ D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D��f111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   @|��@�33@�33A��A=��A]��A}��A���A���A���A���A���A���A���A���BffBffBffBffB'ffB/ffB7ffB?ffBGffBOffBWffB_ffBgffBoffBwffBffB��3B��3B��3B��3B��3B��3B��3B��3B��3B��3B��3B��3B��3B��3B��3B��3Bó3Bǳ3B˳3Bϳ3Bӳ3B׳3B۳3B߳3B�3B�3B�3B�3B�3B��3B��3B��3CٚCٚCٚCٚC	ٚCٚCٚCٚCٚCٚC�4CٚCٚCٚCٚCٚC!ٚC#ٚC%ٚC'ٚC)ٚC+ٚC-�4C/ٚC1ٚC3ٚC5ٚC7ٚC9� C;� C=� C?ٚCAٚCCٚCEٚCGٚCIٚCKٚCMٚCOٚCQٚCSٚCUٚCWٚCYٚC[ٚC]� C_ٚCaٚCcٚCeٚCgٚCiٚCkٚCmٚCoٚCqٚCsٚCuٚCwٚCyٚC{ٚC}ٚCٚC���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C�� C�� C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���D vfD �fDvfD�fDvfD�fDvfD�fDvfD�fDvfD�fDvfD�fDvfD�fDvfD�fD	vfD	�fD
vfD
�fDvfD�fDvfD�fDvfD�fDvfD�fDvfD�fDvfD�fDvfD�fDvfD�fDvfD�fDvfD�fDvfD�fDvfD�fDvfD�fDvfD�fDvfD�fDvfD�fDvfD�fDvfD�fDvfD�fDvfD�fDvfD�fD vfD �fD!vfD!�fD"vfD"� D#p D#�fD$vfD$�fD%vfD%�fD&vfD&�fD'vfD'�fD(vfD(�fD)vfD)�fD*vfD*�fD+vfD+�fD,vfD,�fD-vfD-�fD.vfD.�fD/vfD/�fD0vfD0�fD1vfD1�fD2vfD2�fD3vfD3�fD4vfD4�fD5vfD5�fD6vfD6�fD7vfD7�fD8vfD8�fD9vfD9�fD:vfD:�fD;vfD;�fD<vfD<��D=vfD=�fD>vfD>�fD?vfD?�fD@vfD@�fDAvfDA�fDBvfDB�fDCvfDC�fDDvfDD�fDEvfDE�fDFvfDF�fDGvfDG�fDHvfDH�fDIvfDI�fDJvfDJ�fDKvfDK�fDLvfDL�fDMvfDM�fDNvfDN�fDOvfDO�fDPvfDP�fDQvfDQ�fDRvfDR�fDSvfDS�fDTvfDT�fDUvfDU�fDVvfDV�fDWvfDW�fDXvfDX�fDYvfDY�fDZvfDZ�fD[vfD[�fD\vfD\�fD]vfD]�fD^vfD^�fD_vfD_�fD`vfD`�fDavfDa�fDbvfDb�fDcvfDc�fDdvfDd�fDevfDe�fDfvfDf�fDgvfDg�fDhvfDh�fDivfDi�fDjvfDj�fDkvfDk�fDlvfDl�fDmvfDm�fDnvfDn�fDovfDo�fDpvfDp�fDqvfDq�fDrvfDr�fDsvfDs�fDtvfDt�fDuvfDu�fDvvfDv�fDwvfDw�fDxvfDx�fDyvfDy�fDzvfDz�fD{vfD{�fD|vfD|�fD}vfD}�fD~vfD~�fDvfD�fD�;3D�{3D��3D��3D�;3D�{3D��3D��3D�;3D�{3D��3D��3D�;3D�{3D��3D��3D�;3D�{3D��3D��3D�;3D�{3D��3D��3D�;3D�{3D��3D��3D�;3D�{3D��3D��3D�;3D�{3D��3D��3D�;3D�{3D��3D��3D�;3D�{3D��3D��3D�;3D�{3D��3D��3D�;3D�{3D��3D��3D�;3D�{3D��3D��3D�;3D�{3D��3D��3D�;3D�{3D��3D��3D�;3D�{3D��3D��3D�;3D�{3D��3D��3D�;3D�{3D��3D��3D�;3D�{3D��3D��3D�;3D�{3D��3D��3D�;3D�{3D��3D��3D�;3D�{3D��3D��3D�;3D�{3D��3D��3D�;3D�{3D��3D��3D�;3D�{3D��3D��3D�;3D�{3D��3D��3D�;3D�{3D��3D��3D�;3D�{3D��3D��3D�;3D�{3D��3D��3D�;3D�{3D��3D��3D�;3D�{3D��3D��3D�;3D�{3D��3D��3D�;3D�{3D��3D��3D�;3D�{3D��3D�� D�;3D�{3D��3D��3D�;3D�{3D��3D��3D�;3D�{3D��3D��3D�;3D�{3D��3D��3D�;3D�{3D��3D��3D�;3D�{3D��3D��3D�;3D�{3D��3D��3D�;3D�{3D��3D��3D�;3D�{3D��3D��3D�;3D�{3D��3D��3D�;3D�{3D�� D��3D�;3D�{3D��3D��3D�;3D�{3D��3D��3D�;3D�{3D��3D��3D�;3D�{3D��3D��3D�;3D�{3D��3D��3D�;3D�{3D��3D��3D�;3D�{3D��3D��3D�;3D�{3D��3D��3D�;3D�{3D��3D��3D�;3D�{3D��3D��3D�;3D�{3D��3D��3D�;3D�{3D��3D��3D�;3D�{3D��3D��3D�;3D�{3D��3D��3D�;3D�{3D��3D��3D�;3D�{3D��3D��3D�;3D�{3D��3D��3D�;3D�{3D��3D��3D�;3D�{3D��3D��3D�;3D�{3D��3D��3D�;3D�{3D»3D��3D�;3D�{3Dû3D��3D�;3D�{3DĻ3D��3D�;3D�{3DŻ3D��3D�;3D�{3Dƻ3D��3D�;3D�{3Dǻ3D��3D�;3D�{3DȻ3D��3D�;3D�{3Dɻ3D��3D�;3D�{3Dʻ3D��fD�>fD�{3D˻3D��3D�;3D�{3D̻3D��3D�;3D�{3Dͻ3D��3D�;3D�{3Dλ3D��3D�;3D�{3Dϻ3D��3D�;3D�{3Dл3D��3D�;3D�{3Dѻ3D��3D�;3D�{3Dһ3D��3D�;3D�{3Dӻ3D��3D�;3D�{3DԻ3D��3D�;3D�{3Dջ3D��3D�;3D�{3Dֻ3D��3D�;3D�{3D׻3D��3D�;3D�{3Dػ3D��3D�;3D�{3Dٻ3D��3D�;3D�{3Dڻ3D��3D�;3D�{3Dۻ3D��3D�;3D�{3Dܻ3D��3D�;3D�{3Dݻ3D��3D�;3D�{3D޻3D��3D�;3D�{3D߻3D��3D�;3D�{3D�3D��3D�;3D�{3D�3D��3D�;3D�{3D�3D��3D�;3D�{3D�3D��3D�;3D�{3D�3D��3D�;3D�{3D�3D��3D�;3D�{3D�3D��3D�;3D�{3D�3D��3D�;3D�{3D�3D��3D�;3D�{3D�3D��3D�;3D�{3D�3D��3D�;3D�{3D�3D��3D�;3D�{3D�3D��3D�;3D�{3D��3D��3D�;3D�{3D�3D��3D�;3D�{3D�3D��3D�;3D�{3D�3D��3D�;3D�{3D�3D��3D�;3D�{3D�3D��3D�;3D�{3D�3D��3D�;3D�{3D��3D��3D�;3D�{3D��3D��3D�;3D�{3D��3D��3D�;3D�{3D��3D��3D�;3D�{3D��3D��3D�;3D�{3D��3D��3D�;3D�{3D��3D��3D�;3D���111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�Aд9Aв-Aв-AЬAЩ�AЬAв-Aа!AН�AЅA�x�A�t�A�v�A�x�A�~�AЃAЉ7AЧ�AЮAв-AЮAЧ�AХ�AХ�AХ�AН�AХ�AЩ�Aд9AжFAжFAв-AС�AН�AЛ�AЛ�AЛ�AН�AЛ�AЗ�AЕ�AЏ\AЏ\AЏ\AЏ\AЏ\AБhAЏ\AЋDAЅA�v�Aω7A�5?A�Q�A�I�A���A�A�A��;A�%A��A�G�A�A�A��TA�O�A�x�A���A���A�l�A�p�A�Q�A� �A�VA��hA���A�1A��jA�jA��9A��9A�VA�|�A�l�A��7A�VA���A�{A�~�A�K�A�VA�t�A�C�A�1A�(�A��9A�O�A��A��!A��A��#A�
=A�=qA���A��A��HA���A�-A�5?A��;A��7A���A���A|A�Axz�Av�yAt�As��ArE�Aot�Aj�Ael�AaA_"�AZ��AW��ATjAP�+AK�AJ��AI�-AI+AH�uAF��A@r�A:  A9\)A7S�A6��A6r�A5�mA4��A4=qA3/A1?}A,�A+oA)�FA'�
A'�A&�jA&I�A%��A$�A"v�A"VA!S�A AI�AG�A�!A�AM�A��A�AVA|�A�A5?AVA�A��A|�A/AȴA�!An�A1'A  AO�A��AVA"�AbAG�A
�A
�uA
��A
ffA
{A	��A	hsA	VA�A^5A�A��A��A�#A�/@���@��wA�A �/@�33@��@��R@��@��@��7@�dZ@��@���@��w@�;d@�l�@�bN@�+@�@�|�@�v�@�{@�{@�V@�`B@�w@���@�V@�x�@��/@�u@���@�;d@��@��@�j@��@�"�@�o@�@�!@��@��@��@�Q�@��m@�l�@�
=@�5?@�p�@�/@��/@۾w@�+@ڏ\@٩�@�&�@�Ĝ@��;@׍P@�K�@֟�@���@Դ9@�r�@� �@���@�{@Ѻ^@�7L@�j@υ@�@�
=@���@�v�@�M�@��@Ͳ-@�?}@�Ĝ@̃@�r�@�b@�S�@�o@ʗ�@��@ɑh@�%@��@ǶF@�|�@�K�@���@�^5@�$�@�@��#@���@���@���@ċD@�Z@�b@�|�@���@�@+@�n�@�^5@�M�@�{@��^@�X@�%@���@�bN@��;@�o@��@�v�@�ff@�@���@�G�@���@���@�bN@��@���@��P@�l�@�\)@�
=@��@���@���@�7L@���@���@�I�@�1'@�(�@��m@��F@�l�@�S�@���@�ȴ@�{@���@�O�@�%@�Z@��F@�@�M�@�hs@���@�Z@� �@�1@��@��@���@�ff@��@�/@��`@��@�(�@��m@�dZ@�;d@�^5@��^@�/@���@�r�@�Q�@�9X@��@��
@���@�C�@�o@��@���@�^5@���@�X@�O�@�?}@�V@���@�Q�@���@�\)@�o@��@���@�~�@�J@���@�7L@���@��9@�z�@�A�@��;@���@�|�@�dZ@�@���@��@��-@��h@��7@�X@���@��D@�z�@�Z@�1@�S�@���@�v�@�V@�5?@�{@�@�hs@��j@�r�@��@��@��w@�\)@���@�V@�-@�{@�{@�@��#@��@�O�@�&�@��j@�Z@�1'@��m@��P@�dZ@��R@�v�@�5?@�@���@���@�hs@�%@���@�Z@�1'@� �@�1@��
@��F@���@�\)@��H@���@��!@��+@�=q@���@�&�@���@��D@�Q�@�9X@��@�  @��@��w@�@�v�@�V@�5?@�E�@�J@���@��h@���@�V@���@���@��@���@�Q�@�1@��F@���@�S�@���@���@�M�@�$�@�-@�5?@�$�@�{@��-@�O�@��@�%@��@���@���@���@�z�@�j@�A�@�1@��w@�|�@�l�@�K�@���@�E�@��@���@�`B@�O�@�&�@���@���@���@��@��@���@��/@���@�Z@�A�@�9X@�9X@�9X@��@�  @�@��@�w@\)@
=@~��@~��@~v�@}�T@}�@|�/@|9X@{��@{ƨ@{��@{dZ@{S�@{33@z��@zn�@zJ@yhs@y%@xĜ@xQ�@w�@w��@w+@vȴ@v�+@vV@vE�@v@u@uO�@u`B@t��@t(�@s��@st�@sC�@r��@q�#@qX@q�@p��@pr�@pbN@pA�@p �@p  @o�P@o\)@o�@n�R@nv�@n{@m@m`B@m�@l��@l�D@l�@k��@k"�@j��@jJ@i��@i�@i��@i��@h�u@hb@g��@g\)@gK�@f�y@fv�@fV@f@e/@d�j@d�@d�@d��@d��@d�@c�
@c"�@b�H@b�\@bM�@bM�@a�#@a��@aG�@a�@`��@`�u@_�@_��@_|�@_\)@_;d@_
=@^V@]�@]`B@]/@]V@\�@\�@\�j@\�D@\I�@[ƨ@[C�@Z�\@Y��@Y7L@X��@XĜ@X�u@XbN@Xb@X  @W�@V�@Vff@VE�@V$�@V@U��@U�h@UO�@T�j@T(�@Sƨ@S�@St�@S"�@R-@Q�^@Q�7@P�`@P1'@O��@OK�@O;d@O+@N�y@N��@NE�@N@M/@LZ@K�
@K�@K@J��@J^5@I��@I��@IG�@H��@HA�@G��@F��@E�-@E�@EV@D��@C��@C�F@C��@CdZ@C"�@C@B��@B��@BM�@BJ@A��@A��@A&�@@bN@@A�@@ �@?��@?;d@>ȴ@>@=?}@<��@<�@<1@;�F@;�@;S�@;o@:�H@:M�@9��@9�#@9��@9G�@97L@9%@8Ĝ@8r�@8 �@7|�@7K�@7;d@7
=@6�R@6��@6V@5��@5O�@4�@4�j@4j@3�m@333@3o@2^5@2J@1�^@1��@1x�@1%@0bN@/�@/��@/��@/K�@.�y@.v�@.@-�-@-O�@,�@,�@,�D@,j@,9X@+�
@+t�@+o@*��@*��@*n�@*=q@)�@)��@)&�@(��@(�`@(�`@(��@(�9@(�@(bN@'��@'\)@'�@&�y@&��@&v�@&5?@%�@%@%`B@%/@%V@$�j@$I�@$�@#ƨ@#t�@#33@"��@"n�@"=q@"J@!��@!�7@!x�@!7L@!%@ �9@ �@ A�@  �@�@�P@|�@l�@�@�y@ȴ@�R@ff@ff@$�@�@��@@�h@��@��@�D@9X@�m@�F@t�@C�@33@o@�H@��@�\@-@�#@��@�7@�7@�7@hs@hs@hs@G�@7L@&�@�@%@��@�u@r�@Q�@1'@  @��@�P@K�@��@�R@�+@ff@ff@E�@5?@$�@$�@@@`B@/@V@��@z�@Z@I�@9X@�@��@t�@S�@S�@C�@33@@��@�!@�!@�\@n�@^5@=q@J@�^@hs@G�@7L@�@��@�9@bN@1'@  @�w@l�@;d@
=@
=@�@�@��@��@�@�R@��@��@ff@E�@{@�@@�h@p�@/@�@�@�@�/@z�@(�@��@�
@��@C�@"�@33@C�@S�@"�@
��@
��@
=q@
�@
J@	��@	�@	��@	��@	x�@	hs@	X@	%@��@��@��@�9@�u@1'@�@�w@�P@\)@+@�@�y@�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   Aд9Aв-Aв-AЬAЩ�AЬAв-Aа!AН�AЅA�x�A�t�A�v�A�x�A�~�AЃAЉ7AЧ�AЮAв-AЮAЧ�AХ�AХ�AХ�AН�AХ�AЩ�Aд9AжFAжFAв-AС�AН�AЛ�AЛ�AЛ�AН�AЛ�AЗ�AЕ�AЏ\AЏ\AЏ\AЏ\AЏ\AБhAЏ\AЋDAЅA�v�Aω7A�5?A�Q�A�I�A���A�A�A��;A�%A��A�G�A�A�A��TA�O�A�x�A���A���A�l�A�p�A�Q�A� �A�VA��hA���A�1A��jA�jA��9A��9A�VA�|�A�l�A��7A�VA���A�{A�~�A�K�A�VA�t�A�C�A�1A�(�A��9A�O�A��A��!A��A��#A�
=A�=qA���A��A��HA���A�-A�5?A��;A��7A���A���A|A�Axz�Av�yAt�As��ArE�Aot�Aj�Ael�AaA_"�AZ��AW��ATjAP�+AK�AJ��AI�-AI+AH�uAF��A@r�A:  A9\)A7S�A6��A6r�A5�mA4��A4=qA3/A1?}A,�A+oA)�FA'�
A'�A&�jA&I�A%��A$�A"v�A"VA!S�A AI�AG�A�!A�AM�A��A�AVA|�A�A5?AVA�A��A|�A/AȴA�!An�A1'A  AO�A��AVA"�AbAG�A
�A
�uA
��A
ffA
{A	��A	hsA	VA�A^5A�A��A��A�#A�/@���@��wA�A �/@�33@��@��R@��@��@��7@�dZ@��@���@��w@�;d@�l�@�bN@�+@�@�|�@�v�@�{@�{@�V@�`B@�w@���@�V@�x�@��/@�u@���@�;d@��@��@�j@��@�"�@�o@�@�!@��@��@��@�Q�@��m@�l�@�
=@�5?@�p�@�/@��/@۾w@�+@ڏ\@٩�@�&�@�Ĝ@��;@׍P@�K�@֟�@���@Դ9@�r�@� �@���@�{@Ѻ^@�7L@�j@υ@�@�
=@���@�v�@�M�@��@Ͳ-@�?}@�Ĝ@̃@�r�@�b@�S�@�o@ʗ�@��@ɑh@�%@��@ǶF@�|�@�K�@���@�^5@�$�@�@��#@���@���@���@ċD@�Z@�b@�|�@���@�@+@�n�@�^5@�M�@�{@��^@�X@�%@���@�bN@��;@�o@��@�v�@�ff@�@���@�G�@���@���@�bN@��@���@��P@�l�@�\)@�
=@��@���@���@�7L@���@���@�I�@�1'@�(�@��m@��F@�l�@�S�@���@�ȴ@�{@���@�O�@�%@�Z@��F@�@�M�@�hs@���@�Z@� �@�1@��@��@���@�ff@��@�/@��`@��@�(�@��m@�dZ@�;d@�^5@��^@�/@���@�r�@�Q�@�9X@��@��
@���@�C�@�o@��@���@�^5@���@�X@�O�@�?}@�V@���@�Q�@���@�\)@�o@��@���@�~�@�J@���@�7L@���@��9@�z�@�A�@��;@���@�|�@�dZ@�@���@��@��-@��h@��7@�X@���@��D@�z�@�Z@�1@�S�@���@�v�@�V@�5?@�{@�@�hs@��j@�r�@��@��@��w@�\)@���@�V@�-@�{@�{@�@��#@��@�O�@�&�@��j@�Z@�1'@��m@��P@�dZ@��R@�v�@�5?@�@���@���@�hs@�%@���@�Z@�1'@� �@�1@��
@��F@���@�\)@��H@���@��!@��+@�=q@���@�&�@���@��D@�Q�@�9X@��@�  @��@��w@�@�v�@�V@�5?@�E�@�J@���@��h@���@�V@���@���@��@���@�Q�@�1@��F@���@�S�@���@���@�M�@�$�@�-@�5?@�$�@�{@��-@�O�@��@�%@��@���@���@���@�z�@�j@�A�@�1@��w@�|�@�l�@�K�@���@�E�@��@���@�`B@�O�@�&�@���@���@���@��@��@���@��/@���@�Z@�A�@�9X@�9X@�9X@��@�  @�@��@�w@\)@
=@~��@~��@~v�@}�T@}�@|�/@|9X@{��@{ƨ@{��@{dZ@{S�@{33@z��@zn�@zJ@yhs@y%@xĜ@xQ�@w�@w��@w+@vȴ@v�+@vV@vE�@v@u@uO�@u`B@t��@t(�@s��@st�@sC�@r��@q�#@qX@q�@p��@pr�@pbN@pA�@p �@p  @o�P@o\)@o�@n�R@nv�@n{@m@m`B@m�@l��@l�D@l�@k��@k"�@j��@jJ@i��@i�@i��@i��@h�u@hb@g��@g\)@gK�@f�y@fv�@fV@f@e/@d�j@d�@d�@d��@d��@d�@c�
@c"�@b�H@b�\@bM�@bM�@a�#@a��@aG�@a�@`��@`�u@_�@_��@_|�@_\)@_;d@_
=@^V@]�@]`B@]/@]V@\�@\�@\�j@\�D@\I�@[ƨ@[C�@Z�\@Y��@Y7L@X��@XĜ@X�u@XbN@Xb@X  @W�@V�@Vff@VE�@V$�@V@U��@U�h@UO�@T�j@T(�@Sƨ@S�@St�@S"�@R-@Q�^@Q�7@P�`@P1'@O��@OK�@O;d@O+@N�y@N��@NE�@N@M/@LZ@K�
@K�@K@J��@J^5@I��@I��@IG�@H��@HA�@G��@F��@E�-@E�@EV@D��@C��@C�F@C��@CdZ@C"�@C@B��@B��@BM�@BJ@A��@A��@A&�@@bN@@A�@@ �@?��@?;d@>ȴ@>@=?}@<��@<�@<1@;�F@;�@;S�@;o@:�H@:M�@9��@9�#@9��@9G�@97L@9%@8Ĝ@8r�@8 �@7|�@7K�@7;d@7
=@6�R@6��@6V@5��@5O�@4�@4�j@4j@3�m@333@3o@2^5@2J@1�^@1��@1x�@1%@0bN@/�@/��@/��@/K�@.�y@.v�@.@-�-@-O�@,�@,�@,�D@,j@,9X@+�
@+t�@+o@*��@*��@*n�@*=q@)�@)��@)&�@(��@(�`@(�`@(��@(�9@(�@(bN@'��@'\)@'�@&�y@&��@&v�@&5?@%�@%@%`B@%/@%V@$�j@$I�@$�@#ƨ@#t�@#33@"��@"n�@"=q@"J@!��@!�7@!x�@!7L@!%@ �9@ �@ A�@  �@�@�P@|�@l�@�@�y@ȴ@�R@ff@ff@$�@�@��@@�h@��@��@�D@9X@�m@�F@t�@C�@33@o@�H@��@�\@-@�#@��@�7@�7@�7@hs@hs@hs@G�@7L@&�@�@%@��@�u@r�@Q�@1'@  @��@�P@K�@��@�R@�+@ff@ff@E�@5?@$�@$�@@@`B@/@V@��@z�@Z@I�@9X@�@��@t�@S�@S�@C�@33@@��@�!@�!@�\@n�@^5@=q@J@�^@hs@G�@7L@�@��@�9@bN@1'@  @�w@l�@;d@
=@
=@�@�@��@��@�@�R@��@��@ff@E�@{@�@@�h@p�@/@�@�@�@�/@z�@(�@��@�
@��@C�@"�@33@C�@S�@"�@
��@
��@
=q@
�@
J@	��@	�@	��@	��@	x�@	hs@	X@	%@��@��@��@�9@�u@1'@�@�w@�P@\)@+@�@�y@�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�B�
B�
B�
B�
B�
B�
B�
B�
B�B�B�/B�5B�HB�NB�`B�`B�sB�B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B�BL�BbNBcTBdZBdZBdZBffBcTBffB^5B[#B]/B\)B^5BYBW
BQ�BR�B^5BbNBe`Bn�Bv�Bp�Bk�B[#BF�B0!B�B�B��B��B��B�B�yB�;B��B��B�jB�-B��B��B�uB�JB�7Bx�BjB_;BO�B6FB�BPB
�B
�-B
��B
z�B
O�B
>wB
(�B
1B	�B	�NB	��B	��B	�}B	�B	�oB	t�B	_;B	P�B	<jB	,B	�B	\B��B��B��B�B�B�sB�/B��B��BǮBŢBĜBÖB��B�wB�wB�qBBȴB�B��B	+B	JB	VB	�B	"�B	%�B	B�B	E�B	?}B	E�B	G�B	N�B	VB	`BB	ffB	hsB	cTB	_;B	aHB	`BB	`BB	_;B	`BB	`BB	bNB	cTB	cTB	dZB	e`B	cTB	^5B	[#B	W
B	P�B	R�B	P�B	P�B	Q�B	T�B	\)B	\)B	[#B	ZB	YB	T�B	R�B	O�B	L�B	D�B	=qB	+B	�B	�B	0!B	/B	�B	%B	B	+B	$�B	+B	5?B	@�B	?}B	>wB	:^B	-B	�B	�B	�B	�B	oB	oB	{B	�B	�B	�B	�B	�B	 �B	�B	!�B	$�B	$�B	#�B	$�B	%�B	'�B	)�B	)�B	)�B	,B	-B	-B	.B	0!B	1'B	33B	49B	7LB	;dB	?}B	?}B	A�B	A�B	C�B	L�B	O�B	O�B	N�B	O�B	Q�B	P�B	N�B	L�B	M�B	XB	]/B	[#B	ZB	ZB	YB	\)B	ffB	hsB	k�B	o�B	o�B	p�B	p�B	q�B	q�B	r�B	t�B	v�B	z�B	|�B	�B	�B	�%B	�1B	�DB	�DB	�JB	�\B	�{B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�'B	�!B	�B	�B	�B	�'B	�-B	�3B	�?B	�FB	�RB	�XB	�^B	�dB	�jB	�}B	��B	B	B	B	ÖB	ŢB	ŢB	ƨB	ǮB	ȴB	ɺB	ɺB	ǮB	ƨB	ƨB	ȴB	ȴB	ɺB	ȴB	ǮB	ȴB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�
B	�B	�B	�B	�B	�B	�B	�#B	�#B	�)B	�/B	�/B	�/B	�5B	�;B	�;B	�;B	�;B	�BB	�BB	�BB	�HB	�NB	�NB	�NB	�NB	�TB	�TB	�TB	�ZB	�ZB	�ZB	�`B	�fB	�fB	�fB	�fB	�fB	�mB	�sB	�sB	�sB	�sB	�sB	�sB	�yB	�yB	�yB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
  B
  B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
%B
%B
+B
+B
1B
1B
+B
+B
1B
1B
+B
+B
%B
%B
+B
+B
+B
+B
1B
1B
	7B
	7B
	7B
	7B
	7B
	7B

=B

=B
DB
JB
JB
PB
PB
PB
JB
JB
PB
PB
PB
VB
VB
\B
\B
\B
\B
\B
\B
\B
bB
bB
hB
oB
uB
uB
uB
uB
{B
{B
{B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
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
�B
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
!�B
"�B
!�B
#�B
$�B
$�B
%�B
%�B
%�B
%�B
%�B
%�B
&�B
'�B
(�B
(�B
(�B
)�B
)�B
+B
+B
,B
,B
-B
-B
-B
.B
.B
.B
.B
.B
/B
/B
/B
0!B
/B
0!B
0!B
0!B
0!B
1'B
1'B
1'B
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
6FB
6FB
6FB
6FB
6FB
6FB
6FB
6FB
6FB
7LB
7LB
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
=qB
=qB
=qB
=qB
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
D�B
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
H�B
I�B
I�B
I�B
I�B
J�B
J�B
J�B
J�B
K�B
K�B
L�B
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
S�B
S�B
T�B
T�B
T�B
T�B
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
YB
YB
YB
YB
YB
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
aHB
aHB
aHB
bNB
bNB
cTB
cTB
cTB
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
hsB
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
l�B
l�B
l�B
l�B
l�B
l�B
m�B
m�B
l�B
m�B
m�B
m�B
m�B
m�B
m�B
n�B
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
q�B
q�B
q�B
q�B
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
u�B
u�B
u�B
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
x�B
x�B
x�B
x�B
x�B
x�B
y�B
z�B
z�B
z�B
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
~�B
~�B
~�B
~�B
~�B
~�B
� 111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   B�
B�
B�
B�
B�
B�
B�
B�
B�B�B�/B�5B�HB�NB�`B�`B�sB�B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B�BL�BbNBcTBdZBdZBdZBffBcTBffB^5B[#B]/B\)B^5BYBW
BQ�BR�B^5BbNBe`Bn�Bv�Bp�Bk�B[#BF�B0!B�B�B��B��B��B�B�yB�;B��B��B�jB�-B��B��B�uB�JB�7Bx�BjB_;BO�B6FB�BPB
�B
�-B
��B
z�B
O�B
>wB
(�B
1B	�B	�NB	��B	��B	�}B	�B	�oB	t�B	_;B	P�B	<jB	,B	�B	\B��B��B��B�B�B�sB�/B��B��BǮBŢBĜBÖB��B�wB�wB�qBBȴB�B��B	+B	JB	VB	�B	"�B	%�B	B�B	E�B	?}B	E�B	G�B	N�B	VB	`BB	ffB	hsB	cTB	_;B	aHB	`BB	`BB	_;B	`BB	`BB	bNB	cTB	cTB	dZB	e`B	cTB	^5B	[#B	W
B	P�B	R�B	P�B	P�B	Q�B	T�B	\)B	\)B	[#B	ZB	YB	T�B	R�B	O�B	L�B	D�B	=qB	+B	�B	�B	0!B	/B	�B	%B	B	+B	$�B	+B	5?B	@�B	?}B	>wB	:^B	-B	�B	�B	�B	�B	oB	oB	{B	�B	�B	�B	�B	�B	 �B	�B	!�B	$�B	$�B	#�B	$�B	%�B	'�B	)�B	)�B	)�B	,B	-B	-B	.B	0!B	1'B	33B	49B	7LB	;dB	?}B	?}B	A�B	A�B	C�B	L�B	O�B	O�B	N�B	O�B	Q�B	P�B	N�B	L�B	M�B	XB	]/B	[#B	ZB	ZB	YB	\)B	ffB	hsB	k�B	o�B	o�B	p�B	p�B	q�B	q�B	r�B	t�B	v�B	z�B	|�B	�B	�B	�%B	�1B	�DB	�DB	�JB	�\B	�{B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�'B	�!B	�B	�B	�B	�'B	�-B	�3B	�?B	�FB	�RB	�XB	�^B	�dB	�jB	�}B	��B	B	B	B	ÖB	ŢB	ŢB	ƨB	ǮB	ȴB	ɺB	ɺB	ǮB	ƨB	ƨB	ȴB	ȴB	ɺB	ȴB	ǮB	ȴB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�
B	�B	�B	�B	�B	�B	�B	�#B	�#B	�)B	�/B	�/B	�/B	�5B	�;B	�;B	�;B	�;B	�BB	�BB	�BB	�HB	�NB	�NB	�NB	�NB	�TB	�TB	�TB	�ZB	�ZB	�ZB	�`B	�fB	�fB	�fB	�fB	�fB	�mB	�sB	�sB	�sB	�sB	�sB	�sB	�yB	�yB	�yB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
  B
  B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
%B
%B
+B
+B
1B
1B
+B
+B
1B
1B
+B
+B
%B
%B
+B
+B
+B
+B
1B
1B
	7B
	7B
	7B
	7B
	7B
	7B

=B

=B
DB
JB
JB
PB
PB
PB
JB
JB
PB
PB
PB
VB
VB
\B
\B
\B
\B
\B
\B
\B
bB
bB
hB
oB
uB
uB
uB
uB
{B
{B
{B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
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
�B
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
!�B
"�B
!�B
#�B
$�B
$�B
%�B
%�B
%�B
%�B
%�B
%�B
&�B
'�B
(�B
(�B
(�B
)�B
)�B
+B
+B
,B
,B
-B
-B
-B
.B
.B
.B
.B
.B
/B
/B
/B
0!B
/B
0!B
0!B
0!B
0!B
1'B
1'B
1'B
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
6FB
6FB
6FB
6FB
6FB
6FB
6FB
6FB
6FB
7LB
7LB
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
=qB
=qB
=qB
=qB
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
D�B
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
H�B
I�B
I�B
I�B
I�B
J�B
J�B
J�B
J�B
K�B
K�B
L�B
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
S�B
S�B
T�B
T�B
T�B
T�B
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
YB
YB
YB
YB
YB
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
aHB
aHB
aHB
bNB
bNB
cTB
cTB
cTB
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
hsB
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
l�B
l�B
l�B
l�B
l�B
l�B
m�B
m�B
l�B
m�B
m�B
m�B
m�B
m�B
m�B
n�B
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
q�B
q�B
q�B
q�B
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
u�B
u�B
u�B
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
x�B
x�B
x�B
x�B
x�B
x�B
y�B
z�B
z�B
z�B
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
~�B
~�B
~�B
~�B
~�B
~�B
� 111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�PRES            TEMP            PSAL            PRES_ADJUSTED = PRES - surface_pressure                                                                                                                                                                                                                         none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            surface_pressure=0.15 dbar                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                      Pressure adjusted during real time processing based on most recent valid surface pressure                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                       20230120110148                              AO  ARCAADJP                                                                    20230120110148    IP                G�O�G�O�G�O�                AO  ARGQQCPL                                                                    20230120110148  QCP$                G�O�G�O�G�O�1F83E           AO  ARGQQCPL                                                                    20230120110148  QCF$                G�O�G�O�G�O�0               