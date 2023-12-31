CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       2021-05-19T09:00:40Z creation      
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
_FillValue                 �  I   PRES_ADJUSTED            
      	   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  M    PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  \�   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         decibar    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  `�   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  p4   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  ��   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �h   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         degree_Celsius     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  �T   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  ��   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ��   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  ��   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �,   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  �   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  0  ݼ   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                    ��   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                    ��   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                    ��   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  ,  ��   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                    �   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                    �   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                    �    HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                    �$   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  @  �(   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                    �h   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                    �x   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    �|   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar         �   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar         �   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�        �   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    �Argo profile    3.1 1.2 19500101000000  20210519090040  20210519090040  4903032 US ARGO PROJECT                                                 GREGORY C. JOHNSON                                              PRES            TEMP            PSAL               jA   AO  7212                            2B  A   NAVIS_A                         0942                            170425                          863 @�u��n\1   @�uՈ��F@;rn��O��c��+1   GPS     Primary sampling: averaged                                                                                                                                                                                                                                         jA   A   F   @���@�33A   A   A@  A`  A�  A�  A�  A�  A�  A���A�  A�  B   B  B  B  B   B(  B0  B8  B@  BH  BP  BX  B`  Bh  Bp  Bx  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  C   C  C  C  C  C
  C  C  C  C  C  C  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D&��D'� D(  D(� D)  D)�fD*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dt� Du  Du� Dv  Dv� Dw  Dw� Dx  Dx� Dy  Dy� Dz  Dz� D{  D{� D|  D|� D}  D}� D~  D~� D  D� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D D�� D���D�@ DÀ D�� D�  D�@ DĀ D�� D�  D�@ Dŀ D�� D�  D�@ Dƀ D�� D�  D�@ Dǀ D�� D�  D�@ DȀ D�� D�  D�@ Dɀ D�� D�  D�@ Dʀ D�� D�  D�@ Dˀ D�� D�  D�@ D̀ D�� D�  D�@ D̀ D�� D�  D�@ D΀ D�� D�  D�@ D�|�D�� D�  D�@ DЀ D�� D�  D�@ Dр D�� D�  D�@ DҀ D�� D�  D�@ DӀ D�� D�  D�@ DԀ D�� D�  D�@ DՀ D�� D�  D�@ Dր D�� D�  D�@ D׀ D�� D�  D�@ D؀ D�� D�  D�@ Dـ D�� D�  D�@ Dڀ D�� D�  D�@ Dۀ D�� D�  D�@ D܀ D�� D�  D�@ D݀ D�� D�  D�@ Dހ D�� D�  D�@ D߀ D�� D�  D�@ D�|�D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�3D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D���D�<�D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�3D�C3D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�FfD�ff11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   @���@�ff@�33A��A=��A]��A}��A���A���A���A���Aϙ�A���A���A���BffBffBffBffB'ffB/ffB7ffB?ffBGffBOffBWffB_ffBgffBoffBwffBffB��3B��3B��3B��3B��3B��3B��3B��3B��3B��3B��3B��3B��3B��3B��3B��3Bó3Bǳ3B˳3Bϳ3Bӳ3B׳3B۳3B߳3B�3B�3B�3B�3B�3B��3B��3B��3CٚCٚCٚCٚC	ٚCٚCٚCٚCٚCٚCٚCٚCٚCٚCٚCٚC!ٚC#ٚC%ٚC'ٚC)ٚC+ٚC-ٚC/ٚC1ٚC3ٚC5ٚC7ٚC9ٚC;ٚC=ٚC?ٚCAٚCCٚCEٚCGٚCIٚCKٚCMٚCOٚCQٚCSٚCUٚCWٚCYٚC[ٚC]ٚC_ٚCaٚCcٚCeٚCgٚCiٚCkٚCmٚCoٚCqٚCsٚCuٚCwٚCyٚC{ٚC}ٚCٚC���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���D vfD �fDvfD�fDvfD�fDvfD�fDvfD�fDvfD�fDvfD�fDvfD�fDvfD�fD	vfD	�fD
vfD
�fDvfD�fDvfD�fDvfD�fDvfD�fDvfD�fDvfD�fDvfD�fDvfD�fDvfD�fDvfD�fDvfD�fDvfD�fDvfD�fDvfD�fDvfD�fDvfD�fDvfD�fDvfD�fDvfD�fDvfD�fDvfD�fD vfD �fD!vfD!�fD"vfD"�fD#vfD#�fD$vfD$�fD%vfD%�fD&vfD&� D'vfD'�fD(vfD(�fD)|�D)�fD*vfD*�fD+vfD+�fD,vfD,�fD-vfD-�fD.vfD.�fD/vfD/�fD0vfD0�fD1vfD1�fD2vfD2�fD3vfD3�fD4vfD4�fD5vfD5�fD6vfD6�fD7vfD7�fD8vfD8�fD9vfD9�fD:vfD:�fD;vfD;�fD<vfD<�fD=vfD=�fD>vfD>�fD?vfD?�fD@vfD@�fDAvfDA�fDBvfDB�fDCvfDC�fDDvfDD�fDEvfDE�fDFvfDF�fDGvfDG�fDHvfDH�fDIvfDI�fDJvfDJ�fDKvfDK�fDLvfDL�fDMvfDM�fDNvfDN�fDOvfDO�fDPvfDP�fDQvfDQ�fDRvfDR�fDSvfDS�fDTvfDT�fDUvfDU�fDVvfDV�fDWvfDW�fDXvfDX�fDYvfDY�fDZvfDZ�fD[vfD[�fD\vfD\�fD]vfD]�fD^vfD^�fD_vfD_�fD`vfD`�fDavfDa�fDbvfDb�fDcvfDc�fDdvfDd�fDevfDe�fDfvfDf�fDgvfDg�fDhvfDh�fDivfDi�fDjvfDj�fDkvfDk�fDlvfDl�fDmvfDm�fDnvfDn�fDovfDo�fDpvfDp�fDqvfDq�fDrvfDr�fDsvfDs�fDtvfDt�fDuvfDu�fDvvfDv�fDwvfDw�fDxvfDx�fDyvfDy�fDzvfDz�fD{vfD{�fD|vfD|�fD}vfD}�fD~vfD~�fDvfD�fD�;3D�{3D��3D��3D�;3D�{3D��3D��3D�;3D�{3D��3D��3D�;3D�{3D��3D��3D�;3D�{3D��3D��3D�;3D�{3D��3D��3D�;3D�{3D��3D��3D�;3D�{3D��3D��3D�;3D�{3D��3D��3D�;3D�{3D��3D��3D�;3D�{3D��3D��3D�;3D�{3D��3D��3D�;3D�{3D��3D��3D�;3D�{3D��3D��3D�;3D�{3D��3D��3D�;3D�{3D��3D��3D�;3D�{3D��3D��3D�;3D�{3D��3D��3D�;3D�{3D��3D��3D�;3D�{3D��3D��3D�;3D�{3D��3D��3D�;3D�{3D��3D��3D�;3D�{3D��3D��3D�;3D�{3D��3D��3D�;3D�{3D��3D��3D�;3D�{3D��3D��3D�;3D�{3D��3D��3D�;3D�{3D��3D��3D�;3D�{3D��3D��3D�;3D�{3D��3D��3D�;3D�{3D��3D��3D�;3D�{3D��3D��3D�;3D�{3D��3D��3D�;3D�{3D��3D��3D�;3D�{3D��3D��3D�;3D�{3D��3D��3D�;3D�{3D��3D��3D�;3D�{3D��3D��3D�;3D�{3D��3D��3D�;3D�{3D��3D��3D�;3D�{3D��3D��3D�;3D�{3D��3D��3D�;3D�{3D��3D��3D�;3D�{3D��3D��3D�;3D�{3D��3D��3D�;3D�{3D��3D��3D�;3D�{3D��3D��3D�;3D�{3D��3D��3D�;3D�{3D��3D��3D�;3D�{3D��3D��3D�;3D�{3D��3D��3D�;3D�{3D��3D��3D�;3D�{3D��3D��3D�;3D�{3D��3D��3D�;3D�{3D��3D��3D�;3D�{3D��3D��3D�;3D�{3D��3D��3D�;3D�{3D��3D��3D�;3D�{3D��3D��3D�;3D�{3D��3D��3D�;3D�{3D��3D��3D�;3D�{3D��3D��3D�;3D�{3D��3D��3D�;3D�{3D��3D��3D�;3D�{3D��3D��3D�;3D�{3D��3D��3D�;3D�{3D»3D�� D�;3D�{3Dû3D��3D�;3D�{3DĻ3D��3D�;3D�{3DŻ3D��3D�;3D�{3Dƻ3D��3D�;3D�{3Dǻ3D��3D�;3D�{3DȻ3D��3D�;3D�{3Dɻ3D��3D�;3D�{3Dʻ3D��3D�;3D�{3D˻3D��3D�;3D�{3D̻3D��3D�;3D�{3Dͻ3D��3D�;3D�{3Dλ3D��3D�;3D�x Dϻ3D��3D�;3D�{3Dл3D��3D�;3D�{3Dѻ3D��3D�;3D�{3Dһ3D��3D�;3D�{3Dӻ3D��3D�;3D�{3DԻ3D��3D�;3D�{3Dջ3D��3D�;3D�{3Dֻ3D��3D�;3D�{3D׻3D��3D�;3D�{3Dػ3D��3D�;3D�{3Dٻ3D��3D�;3D�{3Dڻ3D��3D�;3D�{3Dۻ3D��3D�;3D�{3Dܻ3D��3D�;3D�{3Dݻ3D��3D�;3D�{3D޻3D��3D�;3D�{3D߻3D��3D�;3D�x D�3D��3D�;3D�{3D�3D��3D�;3D�{3D�3D��3D�;3D�{3D�3D��3D�;3D�{3D�3D��fD�;3D�{3D�3D��3D�;3D�{3D�3D��3D�;3D�{3D�3D�� D�8 D�{3D�3D��3D�;3D�{3D�3D��3D�;3D�{3D�3D��3D�;3D�{3D�3D��3D�;3D�{3D�3D��fD�>fD�{3D��3D��3D�;3D�{3D�3D��3D�;3D�{3D�3D��3D�;3D�{3D�3D��3D�;3D�{3D�3D��3D�;3D�{3D�3D��3D�;3D�{3D�3D��3D�;3D�{3D��3D��3D�;3D�{3D��3D��3D�;3D�{3D��3D��3D�;3D�{3D��3D��3D�;3D�{3D��3D��3D�;3D�{3D��3D��3D�A�D�a�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�A��RA��hA��A���A�XA��RA�n�A�bNA�Q�A�E�A�?}A�5?A�+A�&�A��A��A��A�ĜA��A��/A�=qA��+A�x�A�|�A�n�A�dZA��A���A��A�-A���A���A���A�dZA��FA�{A�ZA�x�A�A���A�C�A��A��A��A��\A��RA�1'A���A��^A�{A�5?A���A��+A�VA��-A�5?A��A��9A���A�ZA�n�A�JA�+A�(�A��A��A���A��A��A�$�A���A��A�7LA���A��A�JA���A�"�A�p�A��A���A�$�A���A���A��HA�(�A���A�O�A��TA�A�A��A��^A�33A�M�A�p�A��7A�Q�A�  A~�`A}"�A{��Ax�Av5?Au�AuAtI�Ar��ApZAn�+Al�HAk��Aj�yAj��Aj�Aj��AjM�AgAehsAb��AbQ�Aa��A`�HA`I�A_�A_��A_/A^VA]�A\^5A[�A[hsAZȴAY�#AXr�AV�9AV-ATbAR�DAQ��AQG�AP�yAO�mANĜAM7LAJffAH��AH�AF�AF9XAEXAD��AC��AB��AA�A@��A@A�A?\)A>bNA=�TA=��A<�A<ZA<  A;�hA:��A:ĜA:(�A8�yA8bNA8�A7/A6z�A5x�A4�DA4E�A3��A2�A2(�A1K�A0=qA/XA.v�A.bA-ƨA-|�A,jA+33A*ĜA*1'A)��A(��A'��A'S�A&�yA&�+A%�TA%�7A%XA%�A$��A$n�A$1'A#�-A#�A"�DA!�
A!7LA ��A JAl�A��A�TA�yA~�A&�AJA�A�;A"�A�yA�!A^5A�
AdZA&�A�HAr�A  Ap�A�`A�uA^5A�7AXAM�A�AdZA	��AVA�A  AAZA33A�RAt�A �@��@��/@�A�@�7L@�+@���@��@�I�@�\)@��@@�5?@��m@�ff@�t�@�{@�9X@�?}@�t�@�V@ݑh@�X@���@ڏ\@�{@�z�@�33@�5?@�G�@ԣ�@ӍP@��T@�j@��T@̃@˕�@�@Ɵ�@��#@ēu@��@��@�Q�@�K�@�{@���@��@��!@�%@� �@��;@���@���@��T@���@�G�@��`@��u@���@���@��^@���@��9@�1'@��@�1@��@���@��H@�V@���@��`@��@��u@�A�@�C�@�-@��h@�?}@��`@��9@���@�j@��;@�t�@�;d@�@���@�^5@��@���@�V@��u@�I�@���@���@���@��9@��
@��@��y@��@�X@��/@���@�z�@�Z@�(�@��;@�S�@��@�@��H@�v�@�%@�I�@�(�@���@�
=@���@�M�@�=q@�=q@�^5@�~�@��+@��\@�v�@�E�@�{@��@�&�@��@��@��@���@�^5@�J@�x�@��`@�z�@�1'@��;@��@�l�@�"�@��@�V@���@�I�@� �@�  @�  @�  @���@��@��;@���@�t�@�;d@�o@�@��\@�J@�X@�/@�&�@��j@�A�@�(�@�b@�  @���@��;@�l�@�
=@�@��R@�^5@�=q@���@��#@�@��-@���@��@�p�@�hs@�X@��@���@�9X@��;@��@�ƨ@��F@���@��P@�S�@���@���@��!@��+@�E�@�@��#@�@���@��@�hs@�O�@���@�b@�@~��@�P@~�R@~v�@~V@}�h@{ƨ@z�@z�@z=q@y�7@x��@x�9@x�u@xr�@x1'@x  @w��@wl�@v��@u�-@t�D@s�@rn�@qhs@p�u@o\)@o�@n�@nV@m�-@mp�@l�@lI�@l�@kƨ@kdZ@kdZ@kdZ@k"�@j=q@i�@i��@i�7@ihs@i�@g�@gK�@g;d@g+@g�@g�@g
=@f��@f�y@f�y@f�@f��@fV@f$�@e@e?}@d�D@c��@cS�@b�@b��@b��@b�H@ct�@c"�@bM�@aG�@` �@`b@`  @_�@_�w@_�@_�@_\)@_;d@_�@_+@_;d@_;d@_�@_
=@^�@^�R@^V@^{@]��@]�-@]��@]��@]�h@]p�@\��@\(�@\(�@\9X@\�@[�F@[S�@[33@Z��@Z~�@Z^5@Z^5@ZM�@Z=q@Z�@Y��@Y7L@XA�@W��@W�@W�@W+@W+@W+@W
=@Vff@VV@VE�@VE�@VE�@V{@U�@T�@T�/@Tz�@T9X@T�@T�@Sƨ@S�@St�@SdZ@SS�@S33@S@R�!@R=q@RJ@Q��@Q�7@Q�7@Q�7@Q�7@Qx�@QX@Q&�@P�`@Pr�@PQ�@PA�@P1'@P �@O�@O�P@OK�@N�y@Nff@N5?@N@M�-@M�@MO�@L�j@L(�@K�m@K�m@Kƨ@Kƨ@K�
@K��@Kt�@K"�@J�H@J�!@J��@J�\@J=q@I�#@I&�@HĜ@H �@G\)@G+@G+@G
=@F�@F�+@FE�@F@E�@D��@D��@DZ@C��@C��@C33@B�@B��@B^5@B�@A�@A��@A�7@Ax�@A&�@@�u@?�;@?K�@>��@>��@>V@>$�@>@=��@=�h@=O�@=�@=V@<��@<�@<�j@<�D@<Z@<�@;�F@;t�@;C�@;33@:�H@:M�@:=q@:�@9�#@9��@9�@8Ĝ@8�@8bN@8 �@7�;@7�P@7�@6��@6V@65?@6{@6@6@5�@5�T@5�-@5`B@5/@5V@4�@4��@3��@333@2�@2~�@2-@1�^@1hs@1&�@1�@1%@0�u@0�u@0r�@0 �@0b@/�;@/��@/|�@/
=@.ff@.5?@.$�@.{@.@-�@-�-@-O�@-V@,�@,�j@,�@,�D@,z�@,Z@,�@+�
@+C�@*��@*�!@*�!@*�!@*~�@*^5@)�^@)%@(��@(�@(bN@( �@(b@(  @'��@'�@'��@'�P@'|�@&��@&�+@%��@$�j@#ƨ@#t�@#dZ@#33@"�H@"�!@"n�@"^5@"�@!�@!�@!��@!��@!�7@!G�@!7L@!G�@!7L@ ��@ �`@ ��@ �`@ Ĝ@ �9@ Ĝ@ Ĝ@ Ĝ@ ��@ �`@ �`@ Ĝ@ ��@ ��@ �@ Q�@ 1'@ 1'@   @l�@��@�-@O�@�@�@(�@�
@��@t�@dZ@C�@"�@�@��@�!@��@~�@^5@^5@=q@=q@J@hs@7L@&�@&�@&�@%@��@�`@��@��@�u@Q�@  @��@��@\)@;d@
=@$�@��@��@@��@�h@�@`B@/@V@�/@z�@1@�m@�m@�
@�
@�
@�
@�F@S�@"�@�@�H@��@�!@n�@�@J@��@�@�#@��@�^@�^@�^@��@��@��@�^@�^@�^@��@��@�^@�^@�^@�^@��@�7@�7@�7@x�@X@&�@�@��@��@��@r�@bN@Q�@A�@1'@b@l�@��@�@��@{@�h@�@j@I�@9X@I�@9X@9X@9X@(�@(�@9X@1@��@�m@ƨ@�F@�@33@
�\@	��@	X@	7L@	&�@	%@	%@��@��@�9@�u@�@r�@�;@l�@K�@+@�@�@
=@
=@�y@��@ȴ@��@ff@V@E�@5?@5?@$�@{@�T@�-@�@9X@1@��@1@�@1@��@��@1@�
@S�@33@o11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   A��RA��hA��A���A�XA��RA�n�A�bNA�Q�A�E�A�?}A�5?A�+A�&�A��A��A��A�ĜA��A��/A�=qA��+A�x�A�|�A�n�A�dZA��A���A��A�-A���A���A���A�dZA��FA�{A�ZA�x�A�A���A�C�A��A��A��A��\A��RA�1'A���A��^A�{A�5?A���A��+A�VA��-A�5?A��A��9A���A�ZA�n�A�JA�+A�(�A��A��A���A��A��A�$�A���A��A�7LA���A��A�JA���A�"�A�p�A��A���A�$�A���A���A��HA�(�A���A�O�A��TA�A�A��A��^A�33A�M�A�p�A��7A�Q�A�  A~�`A}"�A{��Ax�Av5?Au�AuAtI�Ar��ApZAn�+Al�HAk��Aj�yAj��Aj�Aj��AjM�AgAehsAb��AbQ�Aa��A`�HA`I�A_�A_��A_/A^VA]�A\^5A[�A[hsAZȴAY�#AXr�AV�9AV-ATbAR�DAQ��AQG�AP�yAO�mANĜAM7LAJffAH��AH�AF�AF9XAEXAD��AC��AB��AA�A@��A@A�A?\)A>bNA=�TA=��A<�A<ZA<  A;�hA:��A:ĜA:(�A8�yA8bNA8�A7/A6z�A5x�A4�DA4E�A3��A2�A2(�A1K�A0=qA/XA.v�A.bA-ƨA-|�A,jA+33A*ĜA*1'A)��A(��A'��A'S�A&�yA&�+A%�TA%�7A%XA%�A$��A$n�A$1'A#�-A#�A"�DA!�
A!7LA ��A JAl�A��A�TA�yA~�A&�AJA�A�;A"�A�yA�!A^5A�
AdZA&�A�HAr�A  Ap�A�`A�uA^5A�7AXAM�A�AdZA	��AVA�A  AAZA33A�RAt�A �@��@��/@�A�@�7L@�+@���@��@�I�@�\)@��@@�5?@��m@�ff@�t�@�{@�9X@�?}@�t�@�V@ݑh@�X@���@ڏ\@�{@�z�@�33@�5?@�G�@ԣ�@ӍP@��T@�j@��T@̃@˕�@�@Ɵ�@��#@ēu@��@��@�Q�@�K�@�{@���@��@��!@�%@� �@��;@���@���@��T@���@�G�@��`@��u@���@���@��^@���@��9@�1'@��@�1@��@���@��H@�V@���@��`@��@��u@�A�@�C�@�-@��h@�?}@��`@��9@���@�j@��;@�t�@�;d@�@���@�^5@��@���@�V@��u@�I�@���@���@���@��9@��
@��@��y@��@�X@��/@���@�z�@�Z@�(�@��;@�S�@��@�@��H@�v�@�%@�I�@�(�@���@�
=@���@�M�@�=q@�=q@�^5@�~�@��+@��\@�v�@�E�@�{@��@�&�@��@��@��@���@�^5@�J@�x�@��`@�z�@�1'@��;@��@�l�@�"�@��@�V@���@�I�@� �@�  @�  @�  @���@��@��;@���@�t�@�;d@�o@�@��\@�J@�X@�/@�&�@��j@�A�@�(�@�b@�  @���@��;@�l�@�
=@�@��R@�^5@�=q@���@��#@�@��-@���@��@�p�@�hs@�X@��@���@�9X@��;@��@�ƨ@��F@���@��P@�S�@���@���@��!@��+@�E�@�@��#@�@���@��@�hs@�O�@���@�b@�@~��@�P@~�R@~v�@~V@}�h@{ƨ@z�@z�@z=q@y�7@x��@x�9@x�u@xr�@x1'@x  @w��@wl�@v��@u�-@t�D@s�@rn�@qhs@p�u@o\)@o�@n�@nV@m�-@mp�@l�@lI�@l�@kƨ@kdZ@kdZ@kdZ@k"�@j=q@i�@i��@i�7@ihs@i�@g�@gK�@g;d@g+@g�@g�@g
=@f��@f�y@f�y@f�@f��@fV@f$�@e@e?}@d�D@c��@cS�@b�@b��@b��@b�H@ct�@c"�@bM�@aG�@` �@`b@`  @_�@_�w@_�@_�@_\)@_;d@_�@_+@_;d@_;d@_�@_
=@^�@^�R@^V@^{@]��@]�-@]��@]��@]�h@]p�@\��@\(�@\(�@\9X@\�@[�F@[S�@[33@Z��@Z~�@Z^5@Z^5@ZM�@Z=q@Z�@Y��@Y7L@XA�@W��@W�@W�@W+@W+@W+@W
=@Vff@VV@VE�@VE�@VE�@V{@U�@T�@T�/@Tz�@T9X@T�@T�@Sƨ@S�@St�@SdZ@SS�@S33@S@R�!@R=q@RJ@Q��@Q�7@Q�7@Q�7@Q�7@Qx�@QX@Q&�@P�`@Pr�@PQ�@PA�@P1'@P �@O�@O�P@OK�@N�y@Nff@N5?@N@M�-@M�@MO�@L�j@L(�@K�m@K�m@Kƨ@Kƨ@K�
@K��@Kt�@K"�@J�H@J�!@J��@J�\@J=q@I�#@I&�@HĜ@H �@G\)@G+@G+@G
=@F�@F�+@FE�@F@E�@D��@D��@DZ@C��@C��@C33@B�@B��@B^5@B�@A�@A��@A�7@Ax�@A&�@@�u@?�;@?K�@>��@>��@>V@>$�@>@=��@=�h@=O�@=�@=V@<��@<�@<�j@<�D@<Z@<�@;�F@;t�@;C�@;33@:�H@:M�@:=q@:�@9�#@9��@9�@8Ĝ@8�@8bN@8 �@7�;@7�P@7�@6��@6V@65?@6{@6@6@5�@5�T@5�-@5`B@5/@5V@4�@4��@3��@333@2�@2~�@2-@1�^@1hs@1&�@1�@1%@0�u@0�u@0r�@0 �@0b@/�;@/��@/|�@/
=@.ff@.5?@.$�@.{@.@-�@-�-@-O�@-V@,�@,�j@,�@,�D@,z�@,Z@,�@+�
@+C�@*��@*�!@*�!@*�!@*~�@*^5@)�^@)%@(��@(�@(bN@( �@(b@(  @'��@'�@'��@'�P@'|�@&��@&�+@%��@$�j@#ƨ@#t�@#dZ@#33@"�H@"�!@"n�@"^5@"�@!�@!�@!��@!��@!�7@!G�@!7L@!G�@!7L@ ��@ �`@ ��@ �`@ Ĝ@ �9@ Ĝ@ Ĝ@ Ĝ@ ��@ �`@ �`@ Ĝ@ ��@ ��@ �@ Q�@ 1'@ 1'@   @l�@��@�-@O�@�@�@(�@�
@��@t�@dZ@C�@"�@�@��@�!@��@~�@^5@^5@=q@=q@J@hs@7L@&�@&�@&�@%@��@�`@��@��@�u@Q�@  @��@��@\)@;d@
=@$�@��@��@@��@�h@�@`B@/@V@�/@z�@1@�m@�m@�
@�
@�
@�
@�F@S�@"�@�@�H@��@�!@n�@�@J@��@�@�#@��@�^@�^@�^@��@��@��@�^@�^@�^@��@��@�^@�^@�^@�^@��@�7@�7@�7@x�@X@&�@�@��@��@��@r�@bN@Q�@A�@1'@b@l�@��@�@��@{@�h@�@j@I�@9X@I�@9X@9X@9X@(�@(�@9X@1@��@�m@ƨ@�F@�@33@
�\@	��@	X@	7L@	&�@	%@	%@��@��@�9@�u@�@r�@�;@l�@K�@+@�@�@
=@
=@�y@��@ȴ@��@ff@V@E�@5?@5?@$�@{@�T@�-@�@9X@1@��@1@�@1@��@��@1@�
@S�@33@o11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�B:^B:^B9XB8RB8RB9XB7LB6FB5?B6FB6FB5?B6FB49B33B0!B/B0!BVBe`BffB6FBC�BP�BA�B<jB;dB<jB;dB@�BT�BaHBbNBdZB_;BVBR�BM�BM�BR�BR�BVBVBS�BP�BJ�BB�B;dB+B#�B(�B)�B+B'�B#�B�B �B�B!�B(�B�BoBB�B�/B��BƨBȴB�}B��B�JBt�B[#BK�B@�B2-B+B#�B�BbBB��B�B�;BȴB�dB�3B�B��B��B�VB}�BhsBYBE�B6FB�BB�B�HB�
B��B�-B�B��B��B��B�PB� Bq�BffBZBT�BVBS�BO�BC�B49B$�B�B�B�B�BoBbBPB+BB
��B
��B
��B
�B
�B
�`B
�)B
�
B
��B
ƨB
��B
�}B
�qB
�jB
�LB
�B
��B
��B
��B
��B
��B
�{B
�uB
�bB
�hB
�VB
�JB
�DB
�=B
�7B
�+B
�B
�B
�+B
�1B
�1B
�%B
�B
�B
z�B
v�B
t�B
p�B
n�B
k�B
ffB
hsB
hsB
gmB
aHB
_;B
ZB
VB
R�B
O�B
O�B
N�B
K�B
E�B
D�B
B�B
?}B
>wB
;dB
8RB
7LB
6FB
33B
2-B
1'B
0!B
/B
.B
-B
,B
+B
)�B
&�B
%�B
#�B
"�B
�B
�B
�B
�B
�B
uB
VB
	7B
	7B
%B
%B
%B
B
B
B
B
B
B
  B	��B	��B	��B	��B	��B	��B	�B	�B	�yB	�fB	�TB	�BB	�5B	�#B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	ɺB	ɺB	ȴB	ǮB	ƨB	ĜB	ƨB	ÖB	ÖB	�}B	��B	�wB	�qB	�jB	�jB	�dB	�jB	�jB	�^B	�dB	�^B	�^B	�^B	�^B	�dB	�dB	�^B	�jB	�dB	�dB	�qB	��B	��B	B	B	ÖB	ƨB	ǮB	ȴB	ɺB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�#B	�/B	�;B	�BB	�NB	�NB	�TB	�NB	�ZB	�fB	�mB	�yB	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
B
B
B
B
%B
%B
+B
	7B
DB
\B
hB
oB
�B
�B
�B
�B
�B
�B
�B
 �B
$�B
)�B
+B
+B
+B
+B
49B
:^B
:^B
;dB
<jB
@�B
D�B
F�B
I�B
L�B
O�B
P�B
Q�B
S�B
VB
XB
YB
`BB
cTB
ffB
gmB
o�B
p�B
q�B
t�B
v�B
z�B
{�B
� B
�1B
�=B
�JB
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
�B
�B
�B
�!B
�-B
�3B
�?B
�?B
�FB
�FB
�^B
�jB
�jB
�wB
B
ĜB
ƨB
ǮB
ȴB
ɺB
��B
��B
��B
��B
��B
��B
��B
�B
�B
�)B
�/B
�;B
�HB
�TB
�`B
�sB
�B
�B
�B
�B
�B
��B
��B
��B
��B
��B
��B
��B
��B  BB%B	7B
=B
=BPBoB�B�B�B!�B"�B#�B#�B$�B%�B(�B+B,B-B-B.B0!B2-B33B6FB:^B;dB<jB=qBA�BC�BD�BG�BH�BK�BM�BN�BO�BR�BXBYBZBZB[#B\)B_;BaHBaHBbNBcTBcTBcTBcTBe`Be`Be`BffBgmBhsBiyBk�Bn�Bp�Br�Bs�Bs�Bt�Bu�Bx�By�By�Bx�Bw�Bw�Bw�Bw�Bx�Bx�By�Bz�B{�B{�B{�B{�B{�B{�B{�B|�B}�B~�B� B�B�B�B�B�B�B�B�+B�1B�1B�=B�DB�JB�JB�PB�VB�VB�VB�VB�VB�\B�\B�hB��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B�B�B�B�B�B�B�B�!B�'B�-B�-B�3B�?B�?B�FB�FB�LB�LB�RB�^B�^B�^B�^B�^B�^B�^B�dB�dB�jB�qB�qB�qB�wB�}B��B��BBÖBĜBĜBĜBĜBŢBŢBƨBȴBȴBɺB��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B�B�
B�
B�
B�B�B�B�B�B�B�B�B�B�#B�#B�)B�/B�/B�/B�5B�;B�;B�;B�BB�BB�HB�NB�NB�NB�TB�TB�ZB�ZB�fB�fB�fB�fB�fB�fB�mB�mB�mB�sB�sB�sB�sB�yB�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B  BBBBBBBB%B%B%B%B+B+B+B+B+B1B1B1B1B1B	7B	7B	7B	7B	7B	7B	7B	7B	7B	7B	7B	7B	7B	7B	7B
=B
=B
=B
=B
=BDBJBPBVBVB\BbBbBbBhBhBhBhBoBoBoBoBoBoBoBuBuBuB{B{B{B{B{B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B �B �B �B �B �B �B �B!�B!�B!�B!�B"�B"�B"�B"�B"�B#�B#�B#�B$�B$�B%�B&�B'�B'�B'�B'�B'�B'�B'�B'�B'�B'�B(�B'�B(�B(�B(�B(�B(�B)�B+B,B,B,B,B,B,B-B-B-B-B-B.B.B/B/B/B/B/B/B/B/B/B/B0!B0!B0!B0!B0!B0!B0!B0!B1'B2-B33B33B33B33B33B33B33B33B33B49B49B5?B5?44444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444   B:^B:^B9XB8RB8RB9XB7LB6FB5?B6FB6FB5?B6FB49B33B0!B/B0!BVBe`BffB6FBC�BP�BA�B<jB;dB<jB;dB@�BT�BaHBbNBdZB_;BVBR�BM�BM�BR�BR�BVBVBS�BP�BJ�BB�B;dB+B#�B(�B)�B+B'�B#�B�B �B�B!�B(�B�BoBB�B�/B��BƨBȴB�}B��B�JBt�B[#BK�B@�B2-B+B#�B�BbBB��B�B�;BȴB�dB�3B�B��B��B�VB}�BhsBYBE�B6FB�BB�B�HB�
B��B�-B�B��B��B��B�PB� Bq�BffBZBT�BVBS�BO�BC�B49B$�B�B�B�B�BoBbBPB+BB
��B
��B
��B
�B
�B
�`B
�)B
�
B
��B
ƨB
��B
�}B
�qB
�jB
�LB
�B
��B
��B
��B
��B
��B
�{B
�uB
�bB
�hB
�VB
�JB
�DB
�=B
�7B
�+B
�B
�B
�+B
�1B
�1B
�%B
�B
�B
z�B
v�B
t�B
p�B
n�B
k�B
ffB
hsB
hsB
gmB
aHB
_;B
ZB
VB
R�B
O�B
O�B
N�B
K�B
E�B
D�B
B�B
?}B
>wB
;dB
8RB
7LB
6FB
33B
2-B
1'B
0!B
/B
.B
-B
,B
+B
)�B
&�B
%�B
#�B
"�B
�B
�B
�B
�B
�B
uB
VB
	7B
	7B
%B
%B
%B
B
B
B
B
B
B
  B	��B	��B	��B	��B	��B	��B	�B	�B	�yB	�fB	�TB	�BB	�5B	�#B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	ɺB	ɺB	ȴB	ǮB	ƨB	ĜB	ƨB	ÖB	ÖB	�}B	��B	�wB	�qB	�jB	�jB	�dB	�jB	�jB	�^B	�dB	�^B	�^B	�^B	�^B	�dB	�dB	�^B	�jB	�dB	�dB	�qB	��B	��B	B	B	ÖB	ƨB	ǮB	ȴB	ɺB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�#B	�/B	�;B	�BB	�NB	�NB	�TB	�NB	�ZB	�fB	�mB	�yB	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
B
B
B
B
%B
%B
+B
	7B
DB
\B
hB
oB
�B
�B
�B
�B
�B
�B
�B
 �B
$�B
)�B
+B
+B
+B
+B
49B
:^B
:^B
;dB
<jB
@�B
D�B
F�B
I�B
L�B
O�B
P�B
Q�B
S�B
VB
XB
YB
`BB
cTB
ffB
gmB
o�B
p�B
q�B
t�B
v�B
z�B
{�B
� B
�1B
�=B
�JB
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
�B
�B
�B
�!B
�-B
�3B
�?B
�?B
�FB
�FB
�^B
�jB
�jB
�wB
B
ĜB
ƨB
ǮB
ȴB
ɺB
��B
��B
��B
��B
��B
��B
��B
�B
�B
�)B
�/B
�;B
�HB
�TB
�`B
�sB
�B
�B
�B
�B
�B
��B
��B
��B
��B
��B
��B
��B
��B  BB%B	7B
=B
=BPBoB�B�B�B!�B"�B#�B#�B$�B%�B(�B+B,B-B-B.B0!B2-B33B6FB:^B;dB<jB=qBA�BC�BD�BG�BH�BK�BM�BN�BO�BR�BXBYBZBZB[#B\)B_;BaHBaHBbNBcTBcTBcTBcTBe`Be`Be`BffBgmBhsBiyBk�Bn�Bp�Br�Bs�Bs�Bt�Bu�Bx�By�By�Bx�Bw�Bw�Bw�Bw�Bx�Bx�By�Bz�B{�B{�B{�B{�B{�B{�B{�B|�B}�B~�B� B�B�B�B�B�B�B�B�+B�1B�1B�=B�DB�JB�JB�PB�VB�VB�VB�VB�VB�\B�\B�hB��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B�B�B�B�B�B�B�B�!B�'B�-B�-B�3B�?B�?B�FB�FB�LB�LB�RB�^B�^B�^B�^B�^B�^B�^B�dB�dB�jB�qB�qB�qB�wB�}B��B��BBÖBĜBĜBĜBĜBŢBŢBƨBȴBȴBɺB��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B�B�
B�
B�
B�B�B�B�B�B�B�B�B�B�#B�#B�)B�/B�/B�/B�5B�;B�;B�;B�BB�BB�HB�NB�NB�NB�TB�TB�ZB�ZB�fB�fB�fB�fB�fB�fB�mB�mB�mB�sB�sB�sB�sB�yB�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B  BBBBBBBB%B%B%B%B+B+B+B+B+B1B1B1B1B1B	7B	7B	7B	7B	7B	7B	7B	7B	7B	7B	7B	7B	7B	7B	7B
=B
=B
=B
=B
=BDBJBPBVBVB\BbBbBbBhBhBhBhBoBoBoBoBoBoBoBuBuBuB{B{B{B{B{B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B �B �B �B �B �B �B �B!�B!�B!�B!�B"�B"�B"�B"�B"�B#�B#�B#�B$�B$�B%�B&�B'�B'�B'�B'�B'�B'�B'�B'�B'�B'�B(�B'�B(�B(�B(�B(�B(�B)�B+B,B,B,B,B,B,B-B-B-B-B-B.B.B/B/B/B/B/B/B/B/B/B/B0!B0!B0!B0!B0!B0!B0!B0!B1'B2-B33B33B33B33B33B33B33B33B33B49B49B5?B5?44444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444   G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�PRES            TEMP            PSAL            PRES_ADJUSTED = PRES - surface_pressure                                                                                                                                                                                                                         none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            surface_pressure=0.15 dbar                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                      Pressure adjusted at real time based on most recent valid surface pressure                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                      20210519090040                              AO  ARCAADJP                                                                    20210519090040    IP                G�O�G�O�G�O�                AO  ARGQQCPL                                                                    20210519090040  QCP$                G�O�G�O�G�O�5F03E           AO  ARGQQCPL                                                                    20210519090040  QCF$                G�O�G�O�G�O�8000            