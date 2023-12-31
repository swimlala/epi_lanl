CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       JMA    source        
Argo float     history       b2018-09-08T00:35:23Z creation;2018-09-08T00:35:28Z conversion to V3.1;2019-12-19T07:29:23Z update;     
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
_FillValue                 �  I    PRES_ADJUSTED            
      	   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  M   PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  \�   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     
_FillValue        G�O�   	valid_min         �      	valid_max         B      C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  `l   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  o�   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     
_FillValue        G�O�   	valid_min         �      	valid_max         B      C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  s�   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �X   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    
_FillValue        G�O�   	valid_min         @      	valid_max         B$     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  �<   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ��   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    
_FillValue        G�O�   	valid_min         @      	valid_max         B$     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  ��   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �(   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         decibar    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  �   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         degree_Celsius     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  ��   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  �   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  �  ܘ   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                 	   �(   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                 	   �(   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                 	   �(   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  �  �(   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                    ��   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                    ��   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                    ��   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                    ��   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  @  ��   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                    ��   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                    �   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    �   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar         �   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar         �    HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�        �$   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    �(Argo profile    3.1 1.2 19500101000000  20180908003523  20200116231516  5905048                                                                 JAMSTEC                                                         PRES            TEMP            PSAL              A   JA  I2_0577_279                     2C  D   NAVIS_A                         0577                            ARGO 102115                     863 @���I��1   @����>�@4׆�&���d`(�\1   GPS     A   A   A   Primary sampling: averaged [bin average for 1 Hz CTD]                                                                                                                                                                                                              @�33@���@���A   A@  A`  A�  A���A�  A�  A�  A�  A�  A�  B   B  B  B  B   B(  B0  B8  B@  BH  BP  BX  B`  Bh  Bp  Bx  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B���B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  C   C  C  C  C  C
  C  C  C  C  C  C  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C��C��C��C�  C�  C�  C�  C�  C��C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D��D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DY��DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dt� Du  Du� Dv  Dv� Dw  Dw� Dx  Dx� Dy  Dy� Dz  Dz� D{  D{� D|  D|� D}  D}� D~  D~� D  D� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D��3D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�C3D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D D�� D�  D�@ DÀ D�� D�  D�@ DĀ D�� D�  D�@ Dŀ D�� D�  D�@ Dƀ D�� D�  D�@ Dǀ D�� D�  D�@ DȀ D�� D�  D�@ Dɀ D�� D�  D�@ Dʀ D�� D�  D�@ Dˀ D�� D�  D�@ D̀ D�� D�  D�@ D̀ D�� D�  D�@ D΀ D�� D�  D�@ Dπ D�� D�  D�@ DЀ D�� D�  D�@ Dр D�� D�  D�@ DҀ D�� D�  D�@ DӀ D�� D�  D�@ DԀ D�� D�  D�@ DՀ D�� D�  D�@ Dր D�� D�  D�@ D׀ D�� D�  D�@ D؀ D�� D�  D�@ Dـ D�� D�  D�@ Dڀ D�� D�  D�@ Dۀ D�� D�  D�@ D܀ D�� D�  D�@ D݀ D�� D�  D�@ Dހ D�� D�  D�@ D߀ D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�3D��3D�  D�@ D� D�� D�  D�@ D� D�� D���D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D���D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�C3D�s3111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   @|��@�  @�  A��A=��A]��A}��A���A���A���A���A���A���A���A���BffBffBffBffB'ffB/ffB7ffB?ffBGffBOffBWffB_ffBgffBoffBwffBffB��3B��3B��3B��3B��3B��3B��3B��3B��3B��3B��3B��3B��3B��3B��3B�� Bó3Bǳ3B˳3Bϳ3Bӳ3B׳3B۳3B߳3B�3B�3B�3B�3B�3B��3B��3B��3CٚCٚCٚCٚC	ٚCٚCٚCٚCٚCٚCٚCٚCٚCٚCٚCٚC!ٚC#ٚC%ٚC'ٚC)ٚC+ٚC-ٚC/ٚC1ٚC3ٚC5ٚC7ٚC9ٚC;ٚC=ٚC?ٚCAٚCCٚCEٚCGٚCIٚCKٚCMٚCOٚCQٚCSٚCUٚCWٚCYٚC[ٚC]ٚC_ٚCaٚCcٚCeٚCgٚCiٚCkٚCmٚCoٚCqٚCsٚCuٚCwٚCyٚC{ٚC}ٚCٚC���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C�� C�� C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���D vfD �fDvfD�fDvfD�fDvfD�fDvfD�fDvfD�fDvfD�fDvfD�fDvfD�fD	vfD	�fD
vfD
�fDvfD�fDvfD�fDvfD�fDvfD�fDvfD�fDvfD�fDvfD�fDvfD�fDvfD�fDvfD�fDvfD�fDvfD�fDvfD�fDvfD�fDvfD�fDvfD�fDvfD�fDvfD� DvfD�fDvfD�fDvfD�fD vfD �fD!vfD!�fD"vfD"�fD#vfD#�fD$vfD$�fD%vfD%�fD&vfD&�fD'vfD'�fD(vfD(�fD)vfD)�fD*vfD*�fD+vfD+�fD,vfD,�fD-vfD-�fD.vfD.�fD/vfD/�fD0vfD0�fD1vfD1�fD2vfD2�fD3vfD3�fD4vfD4�fD5vfD5�fD6vfD6�fD7vfD7�fD8vfD8�fD9vfD9�fD:vfD:�fD;vfD;�fD<vfD<�fD=vfD=�fD>vfD>�fD?vfD?�fD@vfD@�fDAvfDA�fDBvfDB�fDCvfDC�fDDvfDD�fDEvfDE�fDFvfDF�fDGvfDG�fDHvfDH�fDIvfDI�fDJvfDJ�fDKvfDK�fDLvfDL�fDMvfDM�fDNvfDN�fDOvfDO�fDPvfDP�fDQvfDQ�fDRvfDR�fDSvfDS�fDTvfDT�fDUvfDU�fDVvfDV�fDWvfDW�fDXvfDX�fDYvfDY� DZvfDZ�fD[vfD[�fD\vfD\�fD]vfD]�fD^vfD^�fD_vfD_�fD`vfD`�fDavfDa�fDbvfDb�fDcvfDc�fDdvfDd�fDevfDe�fDfvfDf�fDgvfDg�fDhvfDh�fDivfDi�fDjvfDj�fDkvfDk�fDlvfDl�fDmvfDm�fDnvfDn�fDovfDo�fDpvfDp�fDqvfDq�fDrvfDr�fDsvfDs�fDtvfDt�fDuvfDu�fDvvfDv�fDwvfDw�fDxvfDx�fDyvfDy�fDzvfDz�fD{vfD{�fD|vfD|�fD}vfD}�fD~vfD~�fDvfD�fD�;3D�{3D��3D��3D�;3D�{3D��3D��3D�;3D�{3D��3D��3D�;3D�{3D��3D��3D�;3D�{3D��3D��3D�;3D�{3D��3D��3D�;3D�{3D��3D��3D�;3D�{3D��3D��3D�;3D�{3D��3D��3D�;3D�{3D��3D��3D�;3D�{3D��3D��3D�;3D�{3D��3D��3D�;3D�{3D��3D��3D�;3D�{3D��3D��3D�;3D�{3D��3D��3D�;3D�{3D��3D��3D�;3D�{3D��3D��3D�;3D�{3D��3D��3D�;3D�{3D��3D��3D�;3D�~fD��3D��3D�;3D�{3D��3D��3D�;3D�{3D��3D��3D�;3D�{3D��3D��3D�;3D�{3D��3D��3D�;3D�{3D��3D��3D�;3D�{3D��3D��3D�;3D�{3D��3D��3D�;3D�{3D��3D��3D�;3D�{3D��3D��3D�;3D�{3D��3D��3D�;3D�{3D��3D��3D�;3D�{3D��3D��3D�;3D�{3D��3D��3D�;3D�{3D��3D��3D�;3D�{3D��3D��3D�;3D�{3D��3D��3D�;3D�{3D��3D��3D�;3D�{3D��3D��3D�;3D�{3D��3D��3D�;3D�{3D��3D��3D�;3D�{3D��3D��3D�;3D�{3D��3D��3D�;3D�{3D��3D��3D�;3D�{3D��3D��3D�;3D�{3D��3D��3D�;3D�{3D��3D��3D�;3D�{3D��3D��3D�;3D�{3D��3D��3D�;3D�{3D��3D��3D�;3D�{3D��3D��3D�;3D�{3D��3D��3D�;3D�{3D��3D��3D�;3D�{3D��3D��3D�;3D�{3D��3D��3D�;3D�{3D��3D��3D�;3D�{3D��3D��3D�;3D�{3D��3D��3D�;3D�{3D��3D��3D�;3D�{3D��3D��3D�;3D�{3D��3D��3D�>fD�{3D��3D��3D�;3D�{3D��3D��3D�;3D�{3D��3D��3D�;3D�{3D��3D��3D�;3D�{3D��3D��3D�;3D�{3D��3D��3D�;3D�{3D»3D��3D�;3D�{3Dû3D��3D�;3D�{3DĻ3D��3D�;3D�{3DŻ3D��3D�;3D�{3Dƻ3D��3D�;3D�{3Dǻ3D��3D�;3D�{3DȻ3D��3D�;3D�{3Dɻ3D��3D�;3D�{3Dʻ3D��3D�;3D�{3D˻3D��3D�;3D�{3D̻3D��3D�;3D�{3Dͻ3D��3D�;3D�{3Dλ3D��3D�;3D�{3Dϻ3D��3D�;3D�{3Dл3D��3D�;3D�{3Dѻ3D��3D�;3D�{3Dһ3D��3D�;3D�{3Dӻ3D��3D�;3D�{3DԻ3D��3D�;3D�{3Dջ3D��3D�;3D�{3Dֻ3D��3D�;3D�{3D׻3D��3D�;3D�{3Dػ3D��3D�;3D�{3Dٻ3D��3D�;3D�{3Dڻ3D��3D�;3D�{3Dۻ3D��3D�;3D�{3Dܻ3D��3D�;3D�{3Dݻ3D��3D�;3D�{3D޻3D��3D�;3D�{3D߻3D��3D�;3D�{3D�3D��3D�;3D�{3D�3D��3D�;3D�{3D�3D��3D�;3D�{3D�3D��3D�;3D�{3D�3D��3D�;3D�{3D�3D��3D�;3D�{3D�3D��3D�;3D�{3D�3D��3D�;3D�~fD�fD��3D�;3D�{3D�3D��3D�;3D�{3D�3D�� D�;3D�{3D�3D��3D�;3D�{3D�3D��3D�;3D�{3D��3D�� D�;3D�{3D�3D��3D�;3D�{3D�3D��3D�;3D�{3D�3D��3D�;3D�{3D�3D��3D�;3D�{3D�3D��3D�;3D�{3D�3D��3D�;3D�{3D��3D��3D�;3D�{3D��3D��3D�;3D�{3D��3D��3D�;3D�{3D��3D��3D�>fD�nf111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   A��A��A��TA��;A��/A��/A��;A��/A��/A��;A��HA��/A��;A��;A��;A��HA��HA��HA��HA��HA��HA��HA��HA��HA��HA��;A��;A��A޼jAޙ�A�`BA�ZAٲ-Aٗ�A���Aכ�A�XA�bA�r�A�{A�bNA�&�A�5?AͬA̕�A� �A��TA���A��
A��A�ȴAǡ�A�n�A��AƾwAŲ-AāAuA�XA��A��;A���A�l�A�
=A���A���A�Q�A��uA�\)A�M�A��+A�p�A�XA��A���A��
A�v�A�bNA���A���A��yA�t�A� �A�ffA�O�A��PA��mA�K�A�/A�(�A�/A�1'A�bA���A�\)A���A�C�A�z�A�`BA�hsA�r�A�M�A�ZA�E�A��A�&�A�~�A�
=A�33A��
A�M�A��A��uA��A�r�A�?}A�5?A��A��A�XA�oA�r�A��A�/A}�^A{�^Ax��ApjAnA�Aj�/Ag&�Ae"�Acl�AbJA`1A]��A[�^AX��AUS�AR^5AQS�AP�APz�AP5?ANĜALbAH�yAF��AD �AC�ABĜAB5?A@��A?�#A>ĜA;�mA8��A8VA7�A7"�A6-A61A5;dA2�A1�^A0�A/�TA.E�A,�!A,E�A+�A)�FA(�A(��A'��A'�A&-A%VA#�mA!�;A 9XAK�A^5AA�A�AI�AS�A��A�wAM�Ar�A�TA�wA�A�A;dA�RA��A��A�DAƨA��AXA�/A��AVA?}A
��A
bA	"�AQ�A��AVAZA�wAK�A�+A�
A�HA9XA�A�A"�A �@�S�A �@�t�@�G�@�r�@�b@�C�@���@��;@�x�@�@��@��@�M�@�p�@@�J@�%@��@�p�@�Ĝ@� �@�ƨ@�5?@� �@�~�@�r�@�@�dZ@��@���@���@�C�@�@�\)@��@�R@�$�@ᙚ@߮@��@�G�@ۍP@�5?@��T@� �@�bN@�+@�@��/@��@�^5@��@��@�=q@�C�@�E�@Ĵ9@��;@���@�ff@ǅ@�|�@�l�@Ɵ�@���@���@�O�@Ĵ9@î@�;d@��@�E�@�@��@��^@��@�&�@�b@��P@��@�@���@�/@��@�9X@�\)@�v�@���@�@�33@��@�{@�?}@�j@�5?@��@��
@� �@��m@�K�@��!@�$�@��@��T@���@�7L@��`@��9@���@�Ĝ@���@���@�z�@��m@��P@�K�@�5?@���@�O�@�V@��9@�j@�1'@��@��F@��P@�K�@��R@�=q@��#@���@��#@���@�?}@��@�I�@��F@�l�@�;d@��@���@�ff@�5?@��@��T@���@���@�?}@��@��j@��@���@��@�r�@�1'@���@���@�t�@�l�@�dZ@���@��R@��\@�ff@�-@�J@���@���@��@�G�@���@�j@��@��@���@��w@��F@���@��P@�t�@�\)@�"�@���@��\@��\@�~�@�M�@���@��-@�`B@��9@�z�@�Q�@�(�@���@�ƨ@��@�C�@�+@��y@���@��\@�^5@�@��@���@��@�p�@��@��9@�(�@���@�1@���@��m@��;@���@�ƨ@�ƨ@���@�t�@�C�@��@���@���@�ȴ@���@�ȴ@���@���@���@�ff@��^@�hs@��@��u@�j@��@�\)@�"�@�
=@���@��!@�~�@�M�@�5?@�@�G�@�&�@���@���@�z�@�r�@��@���@��@��;@��F@�\)@�;d@�"�@���@�5?@�J@��#@���@���@�`B@�O�@�?}@��@��/@�r�@��w@���@�$�@���@��h@�V@��j@�z�@�1'@��@�1@�ƨ@�K�@�"�@�@��@��@�ȴ@��!@��+@�V@�5?@�{@�@��#@�p�@�/@�V@���@��`@���@�r�@�1'@� �@�b@l�@~��@~�R@~�+@~V@~{@}�@}�T@}��@}�@}?}@}�@}?}@|��@|z�@|z�@|�j@|��@|z�@{�m@{ƨ@{��@{C�@z��@y��@yhs@x��@xr�@xQ�@x1'@xb@w��@v�@v5?@u��@u�h@uO�@u/@uV@t�/@tj@t(�@s�@so@r=q@q��@q�@p�9@pA�@pb@p �@p �@o\)@n��@nV@m@m?}@l��@l�j@l�@l��@l�@k"�@jn�@ihs@h�`@hr�@hA�@h �@g�@g|�@f�+@f{@e��@eV@dI�@c��@c��@cdZ@b��@b=q@b-@b�@a�#@a�^@ahs@a�@`�u@`A�@_�;@_+@^�@^�R@^�+@^E�@^{@]�-@]`B@\��@\1@[dZ@[33@[@Z�!@Z~�@Y�@Y�^@Yx�@Y%@X�@XQ�@XA�@Xb@W�w@W��@W��@W�P@W|�@WK�@V�@V{@U��@U�@U�T@U�h@U?}@U/@UV@T�j@T�j@T��@Tz�@TI�@Sƨ@So@Q�@Qx�@QX@QG�@Q7L@Q&�@Q�@Q�@Q%@P��@P�@O�w@O�@O��@O|�@O;d@O+@O
=@N��@N��@NV@NE�@N5?@N{@M�@M@M��@M`B@L�/@Lj@L9X@K�
@Kƨ@KdZ@J�@J�H@J�!@J�\@J~�@JM�@I�#@IX@I�@H�`@H�`@H��@H��@HA�@Gl�@F��@F�@F�R@F��@Fv�@FV@FE�@F@E��@E?}@EV@D��@D�@Ct�@C@B�H@B��@Bn�@B-@A�@A��@A�7@Ahs@A&�@@Ĝ@@�u@@�@@Q�@?�w@?K�@>�@>��@>5?@>{@=�@=@=�h@=?}@<�@<�/@<��@<��@<�j@<�j@<�j@<��@<Z@<�@;��@;�m@;�
@;��@;dZ@;S�@;"�@:��@:�!@:��@:�\@:^5@:M�@:J@9hs@8�9@8bN@8A�@7��@7K�@7+@7+@7�@7�@6�y@6v�@6E�@5��@5�h@5?}@4�/@4z�@3�
@3��@3t�@3C�@3@2~�@1�@1�7@1X@17L@17L@1&�@1�@1%@0�`@0�9@0��@0�u@0r�@/�@/\)@/�@.�y@.��@.��@.�+@.V@.5?@.$�@-��@-p�@-/@-V@,�@,9X@,�@+��@+��@+�m@+�F@+t�@+"�@*��@*M�@*=q@*J@)�#@)��@)x�@)G�@)&�@)�@)%@(�`@(�`@(��@(Ĝ@(�9@(r�@(A�@'��@'��@'K�@&�y@&�+@&5?@&@%�@%��@%`B@%�@$��@$j@$9X@#�
@#��@#�@#S�@"��@"=q@!��@!��@!��@!��@!x�@!G�@!7L@!%@ r�@ b@�;@�w@��@;d@�y@�@�@�@�@�@�+@E�@@@p�@O�@O�@�@�/@z�@9X@�@�m@�
@�F@��@t�@C�@o@�H@��@��@~�@^5@=q@��@�@x�@7L@��@Ĝ@�9@�u@bN@Q�@A�@1'@  @|�@;d@�@
=@��@ȴ@�+@ff@5?@�@�-@�h@`B@��@�/@��@�j@�D@j@I�@��@ƨ@dZ@33@�@�\@M�@M�@=q@=q@J@�@�@�#@��@��@hs@%@��@Ĝ@�9@�@Q�@ �@  @�;@�@�P@|�@l�@|�@|�@l�@K�@+@�R@ff@{111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   A��A��A��TA��;A��/A��/A��;A��/A��/A��;A��HA��/A��;A��;A��;A��HA��HA��HA��HA��HA��HA��HA��HA��HA��HA��;A��;A��A޼jAޙ�A�`BA�ZAٲ-Aٗ�A���Aכ�A�XA�bA�r�A�{A�bNA�&�A�5?AͬA̕�A� �A��TA���A��
A��A�ȴAǡ�A�n�A��AƾwAŲ-AāAuA�XA��A��;A���A�l�A�
=A���A���A�Q�A��uA�\)A�M�A��+A�p�A�XA��A���A��
A�v�A�bNA���A���A��yA�t�A� �A�ffA�O�A��PA��mA�K�A�/A�(�A�/A�1'A�bA���A�\)A���A�C�A�z�A�`BA�hsA�r�A�M�A�ZA�E�A��A�&�A�~�A�
=A�33A��
A�M�A��A��uA��A�r�A�?}A�5?A��A��A�XA�oA�r�A��A�/A}�^A{�^Ax��ApjAnA�Aj�/Ag&�Ae"�Acl�AbJA`1A]��A[�^AX��AUS�AR^5AQS�AP�APz�AP5?ANĜALbAH�yAF��AD �AC�ABĜAB5?A@��A?�#A>ĜA;�mA8��A8VA7�A7"�A6-A61A5;dA2�A1�^A0�A/�TA.E�A,�!A,E�A+�A)�FA(�A(��A'��A'�A&-A%VA#�mA!�;A 9XAK�A^5AA�A�AI�AS�A��A�wAM�Ar�A�TA�wA�A�A;dA�RA��A��A�DAƨA��AXA�/A��AVA?}A
��A
bA	"�AQ�A��AVAZA�wAK�A�+A�
A�HA9XA�A�A"�A �@�S�A �@�t�@�G�@�r�@�b@�C�@���@��;@�x�@�@��@��@�M�@�p�@@�J@�%@��@�p�@�Ĝ@� �@�ƨ@�5?@� �@�~�@�r�@�@�dZ@��@���@���@�C�@�@�\)@��@�R@�$�@ᙚ@߮@��@�G�@ۍP@�5?@��T@� �@�bN@�+@�@��/@��@�^5@��@��@�=q@�C�@�E�@Ĵ9@��;@���@�ff@ǅ@�|�@�l�@Ɵ�@���@���@�O�@Ĵ9@î@�;d@��@�E�@�@��@��^@��@�&�@�b@��P@��@�@���@�/@��@�9X@�\)@�v�@���@�@�33@��@�{@�?}@�j@�5?@��@��
@� �@��m@�K�@��!@�$�@��@��T@���@�7L@��`@��9@���@�Ĝ@���@���@�z�@��m@��P@�K�@�5?@���@�O�@�V@��9@�j@�1'@��@��F@��P@�K�@��R@�=q@��#@���@��#@���@�?}@��@�I�@��F@�l�@�;d@��@���@�ff@�5?@��@��T@���@���@�?}@��@��j@��@���@��@�r�@�1'@���@���@�t�@�l�@�dZ@���@��R@��\@�ff@�-@�J@���@���@��@�G�@���@�j@��@��@���@��w@��F@���@��P@�t�@�\)@�"�@���@��\@��\@�~�@�M�@���@��-@�`B@��9@�z�@�Q�@�(�@���@�ƨ@��@�C�@�+@��y@���@��\@�^5@�@��@���@��@�p�@��@��9@�(�@���@�1@���@��m@��;@���@�ƨ@�ƨ@���@�t�@�C�@��@���@���@�ȴ@���@�ȴ@���@���@���@�ff@��^@�hs@��@��u@�j@��@�\)@�"�@�
=@���@��!@�~�@�M�@�5?@�@�G�@�&�@���@���@�z�@�r�@��@���@��@��;@��F@�\)@�;d@�"�@���@�5?@�J@��#@���@���@�`B@�O�@�?}@��@��/@�r�@��w@���@�$�@���@��h@�V@��j@�z�@�1'@��@�1@�ƨ@�K�@�"�@�@��@��@�ȴ@��!@��+@�V@�5?@�{@�@��#@�p�@�/@�V@���@��`@���@�r�@�1'@� �@�b@l�@~��@~�R@~�+@~V@~{@}�@}�T@}��@}�@}?}@}�@}?}@|��@|z�@|z�@|�j@|��@|z�@{�m@{ƨ@{��@{C�@z��@y��@yhs@x��@xr�@xQ�@x1'@xb@w��@v�@v5?@u��@u�h@uO�@u/@uV@t�/@tj@t(�@s�@so@r=q@q��@q�@p�9@pA�@pb@p �@p �@o\)@n��@nV@m@m?}@l��@l�j@l�@l��@l�@k"�@jn�@ihs@h�`@hr�@hA�@h �@g�@g|�@f�+@f{@e��@eV@dI�@c��@c��@cdZ@b��@b=q@b-@b�@a�#@a�^@ahs@a�@`�u@`A�@_�;@_+@^�@^�R@^�+@^E�@^{@]�-@]`B@\��@\1@[dZ@[33@[@Z�!@Z~�@Y�@Y�^@Yx�@Y%@X�@XQ�@XA�@Xb@W�w@W��@W��@W�P@W|�@WK�@V�@V{@U��@U�@U�T@U�h@U?}@U/@UV@T�j@T�j@T��@Tz�@TI�@Sƨ@So@Q�@Qx�@QX@QG�@Q7L@Q&�@Q�@Q�@Q%@P��@P�@O�w@O�@O��@O|�@O;d@O+@O
=@N��@N��@NV@NE�@N5?@N{@M�@M@M��@M`B@L�/@Lj@L9X@K�
@Kƨ@KdZ@J�@J�H@J�!@J�\@J~�@JM�@I�#@IX@I�@H�`@H�`@H��@H��@HA�@Gl�@F��@F�@F�R@F��@Fv�@FV@FE�@F@E��@E?}@EV@D��@D�@Ct�@C@B�H@B��@Bn�@B-@A�@A��@A�7@Ahs@A&�@@Ĝ@@�u@@�@@Q�@?�w@?K�@>�@>��@>5?@>{@=�@=@=�h@=?}@<�@<�/@<��@<��@<�j@<�j@<�j@<��@<Z@<�@;��@;�m@;�
@;��@;dZ@;S�@;"�@:��@:�!@:��@:�\@:^5@:M�@:J@9hs@8�9@8bN@8A�@7��@7K�@7+@7+@7�@7�@6�y@6v�@6E�@5��@5�h@5?}@4�/@4z�@3�
@3��@3t�@3C�@3@2~�@1�@1�7@1X@17L@17L@1&�@1�@1%@0�`@0�9@0��@0�u@0r�@/�@/\)@/�@.�y@.��@.��@.�+@.V@.5?@.$�@-��@-p�@-/@-V@,�@,9X@,�@+��@+��@+�m@+�F@+t�@+"�@*��@*M�@*=q@*J@)�#@)��@)x�@)G�@)&�@)�@)%@(�`@(�`@(��@(Ĝ@(�9@(r�@(A�@'��@'��@'K�@&�y@&�+@&5?@&@%�@%��@%`B@%�@$��@$j@$9X@#�
@#��@#�@#S�@"��@"=q@!��@!��@!��@!��@!x�@!G�@!7L@!%@ r�@ b@�;@�w@��@;d@�y@�@�@�@�@�@�+@E�@@@p�@O�@O�@�@�/@z�@9X@�@�m@�
@�F@��@t�@C�@o@�H@��@��@~�@^5@=q@��@�@x�@7L@��@Ĝ@�9@�u@bN@Q�@A�@1'@  @|�@;d@�@
=@��@ȴ@�+@ff@5?@�@�-@�h@`B@��@�/@��@�j@�D@j@I�@��@ƨ@dZ@33@�@�\@M�@M�@=q@=q@J@�@�@�#@��@��@hs@%@��@Ĝ@�9@�@Q�@ �@  @�;@�@�P@|�@l�@|�@|�@l�@K�@+@�R@ff@{111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   B	�RB	�RB	�XB	�^B	�^B	�^B	�^B	�^B	�^B	�^B	�^B	�^B	�^B	�^B	�^B	�^B	�^B	�^B	�^B	�^B	�^B	�^B	�^B	�^B	�XB	�XB	�LB	�9B	�'B	��B	��B	�B
hB
PB
B
-B
E�B
_;B
H�B
33B
PB
N�B
q�B
s�B
� B
� B
|�B
�hB
�ZB�B'�B(�B$�B1'B)�B,B�BDB�B!�B�B(�B,B0!B.B6FB^5Bu�Bw�BgmBhsBXB�+B�JBm�Bw�B�%B�DB�7B�DB��B��B��B�JB��B�%B��B�^B�wB��B�wB�dB�wB��B��B�LB�wB��B�Bn�BH�B�B
�;B
�ZB
�B
��B
�fB
��B
�B
VB
VB
[#B
<jB
.B
!�B	��B	�B	ǮB	�B	�}B	�uB	l�B	?}B	/B	.B	�B��B�B�TB��B�5B��B��BB�LB�!B��B�7B��B�RB�jB�}B�9B��B� B�+B�uB��B�?BĜB�}B�3B�wB�XB��B��B��B��B��BɺB��BŢB�'BBÖBĜB�dB�XB��BɺB��B��B�)B��B��B��BĜBÖB�-B�?B�LB�-B��B��B��B�bB�7B�Bo�BjBw�B�B�B|�Bw�Bq�BgmB^5B]/Bo�Bz�Bw�Bv�B}�By�Bo�B~�Bw�Br�Bv�By�Bz�By�Bz�B{�Bw�Bz�Bv�B}�B�B~�B|�Bx�B�B��B�\B�B�oB��B�{B�VB�VB�bB��B�B��B�B�B��B��B�B�LB�qB�FB�B�B��B��B��B��B�?B��B��BǮB��B��B��B��B��B��B��BǮB�qBȴB��BĜB�}B�3B�-BB�XB��B�B��B�FB�dB�3B�dB��B��BǮB��B�BB��B��B��B��B��B	  B		7B	B	B��B	  B	B	B	B	+B	%B	B	B��B��B	B��B	+B	+B	B	B��B��B	  B��B�B�B	B	B��B��B��B		7B	�B	uB	bB	hB	{B	�B	�B	�B	�B	�B	!�B	(�B	)�B	)�B	(�B	&�B	'�B	1'B	/B	+B	.B	49B	6FB	8RB	;dB	>wB	@�B	B�B	H�B	J�B	I�B	J�B	P�B	\)B	\)B	[#B	ZB	]/B	YB	]/B	e`B	jB	m�B	n�B	q�B	w�B	y�B	|�B	}�B	|�B	{�B	� B	�B	�+B	�+B	�+B	�+B	�%B	�+B	�JB	�VB	�\B	�\B	�PB	�bB	�uB	�uB	�{B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�!B	�-B	�'B	�B	�!B	�!B	�-B	�'B	�XB	�dB	�dB	�dB	�qB	�qB	�wB	��B	�}B	B	ÖB	ĜB	ĜB	ǮB	ƨB	ȴB	ɺB	ǮB	ƨB	ɺB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�
B	�B	�B	�)B	�)B	�)B	�5B	�/B	�)B	�/B	�#B	�B	�5B	�;B	�5B	�NB	�NB	�;B	�mB	�yB	�sB	�yB	�yB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	��B	��B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
  B
  B
B	��B	��B
  B
B
B
B
B
B
B
%B
%B
B
%B
1B
	7B
	7B
	7B

=B
DB

=B
	7B
DB
PB
VB
JB
VB
oB
{B
�B
uB
oB
�B
�B
uB
oB
bB
{B
uB
�B
�B
�B
�B
{B
{B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
 �B
 �B
 �B
�B
�B
�B
�B
 �B
 �B
!�B
!�B
 �B
�B
�B
!�B
"�B
!�B
!�B
%�B
%�B
%�B
%�B
&�B
)�B
)�B
(�B
)�B
(�B
(�B
'�B
(�B
(�B
&�B
)�B
+B
+B
)�B
+B
)�B
)�B
(�B
'�B
)�B
-B
-B
-B
.B
-B
/B
/B
.B
.B
0!B
1'B
1'B
1'B
2-B
33B
33B
2-B
2-B
1'B
0!B
33B
6FB
6FB
5?B
6FB
7LB
7LB
7LB
8RB
8RB
8RB
7LB
5?B
5?B
49B
9XB
;dB
<jB
<jB
<jB
<jB
<jB
<jB
;dB
9XB
9XB
=qB
=qB
=qB
<jB
>wB
=qB
=qB
=qB
<jB
>wB
>wB
>wB
>wB
=qB
=qB
=qB
=qB
<jB
>wB
>wB
?}B
>wB
>wB
@�B
@�B
A�B
A�B
@�B
?}B
@�B
B�B
B�B
C�B
C�B
B�B
A�B
@�B
B�B
E�B
E�B
E�B
E�B
E�B
E�B
D�B
D�B
D�B
E�B
C�B
D�B
C�B
E�B
F�B
F�B
E�B
E�B
F�B
G�B
I�B
H�B
H�B
G�B
I�B
I�B
H�B
G�B
G�B
H�B
I�B
I�B
J�B
J�B
J�B
J�B
J�B
K�B
L�B
L�B
L�B
L�B
L�B
L�B
L�B
K�B
L�B
L�B
M�B
M�B
L�B
L�B
M�B
M�B
L�B
M�B
N�B
M�B
M�B
M�B
M�B
K�B
K�B
N�B
O�B
N�B
N�B
P�B
Q�B
Q�B
Q�B
P�B
O�B
O�B
O�B
P�B
P�B
O�B
P�B
P�B
S�B
S�B
S�B
S�B
R�B
R�B
T�B
VB
W
B
XB
XB
XB
XB
W
B
W
B
XB
XB
W
B
VB
T�B
XB
YB
YB
ZB
ZB
YB
ZB
ZB
YB
YB
ZB
ZB
ZB
ZB
\)B
]/B
]/B
]/B
\)B
[#B
[#B
[#B
[#B
]/B
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
_;B
_;B
_;B
^5B
^5B
]/B
_;B
^5B
^5B
^5B
_;B
`BB
aHB
`BB
`BB
`BB
_;B
aHB
aHB
`BB
bNB
bNB
aHB
`BB
`BB
bNB
dZB
e`B
e`B
dZB
dZB
dZB
cTB
bNB
cTB
e`B
e`B
e`B
dZB
e`B
hsB
hsB
hsB
gmB
gmB
e`B
e`B
ffB
ffB
gmB
hsB
hsB
gmB
gmB
gmB
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
iyB
jB
iyB
iyB
k�B
k�B
k�B
k�B
k�B
l�B
l�B
l�B
k�B
jB
l�B
m�B
m�B
m�B
l�B
l�B
m�B
m�B
m�B
m�B
n�B
n�B
m�B
n�B
o�B
o�B
n�B
o�B
o�B
n�B
o�B
o�B
o�B
p�B
p�B
p�B
r�B
r�B
r�B
q�B
r�B
r�B
r�B
r�B
q�B
q�B
p�B
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
t�B
t�B
u�B
t�B
t�B
t�B
s�B
s�B
s�B
t�B
t�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   B	�lB	�lB	��B	�^B	�^B	�^B	�xB	�xB	�^B	�^B	�xB	�xB	�^B	�xB	�^B	�xB	�^B	�xB	�xB	�^B	�xB	�^B	�^B	�xB	��B	��B	��B	��B	�GB	�:B	�CB	�B
B
BB
1B
/OB
G+B
b�B
MB
5tB
�B
P�B
r�B
u�B
� B
�;B
�B
��B
�`BB(XB)�B&B2B,qB.�B�B~B \B#�B�B)�B-)B1vB0;B9rB_pBvFBxlBi�BkB\)B�1B�jBq�Bz�B�KB��B�xB�PB��B��B�yB�B��B�RB��B�^B�wB��B��B�B�.BBΊB��B�OB�xB��Br�BK�B!�B
��B
�fB
�B
��B
�>B
��B
��B
[qB
X�B
\�B
@iB
1�B
$�B	��B	��B	�6B	�B	��B	�B	r�B	F?B	4nB	1AB	!HB�]B�B�RB�?B�B�FB��B�mB�xB�B�xB��B��B��B�VB� B�?B�!B��B��B�SB��B�`B�9B��B�tB��B�B��B��B�:BҽB��B�BуB�EB�B��B�B��B��B�0B�}B�B��B��B��B�hB��B�BB�tB�SB�B�LB��B��B�eB�?B��B��B�rB��Bq�Bl�Bx�B�;B�;B}qBx�Br�BiB`�B_Bp�B{0Bx�Bw�B~BBz�Bq[B�By>BtBxBz�B{�Bz�B{�B|�By$B|Bx8B~�B��B�B}�Bz*B��B�kB�.B��B��B�
B�gB��B��B� B��B��B��B��B��B�$B�B��B�fB�qB��B��B��B�LB�BB�,B�XB��B��B��B��BˬB̳B��B��B�.B�VB�JBȀB��B�7B��B�B��B��B��B��B��B�B��B�B��B��B��B��B̘B̳B��B�HB߾B��B��B�B�DB��B	 �B		RB	�B	�B��B	 �B	{B	�B	mB	zB	tB	mB	�B��B�}B	�B��B	_B	�B	�B	{B��B��B	 �B�qB�iB�B	B	�B��B�`B�	B		�B	mB	�B	�B	�B	�B	�B	�B	�B	B	B	!�B	)B	*B	*B	)*B	'mB	(�B	1vB	/�B	+�B	.�B	4�B	6�B	8�B	;�B	>�B	@�B	B�B	IB	K)B	J=B	KDB	QNB	\)B	\]B	[qB	Z�B	]~B	Y�B	]�B	e�B	j�B	m�B	o B	rB	xB	zB	}"B	~B	}"B	|PB	�4B	�SB	�_B	�_B	�_B	�_B	�tB	��B	�~B	��B	��B	�vB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�)B	�B	�B	�B	�
B	�*B	�*B	�*B	�*B	�B	�B	�>B	�_B	�!B	�GB	�AB	�iB	��B	�oB	�|B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	��B	��B	��B	��B	�B	�+B	�	B	��B	�B	�,B	�2B	�B	�2B	�2B	�2B	�FB	�2B	�YB	�_B	�QB	�CB	�CB	�CB	�OB	�dB	�]B	�dB	�qB	ٴB	ބB	ߊB	ބB	�B	�B	߾B	�B	�B	��B	�B	��B	��B	��B	��B	��B	��B	��B	��B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�'B	�B	��B	��B	��B	�B	��B	�	B	�	B	�B	�B	�B	�;B	�OB	�-B	�B	�%B	�B	��B	�B	�	B	��B	�B	�*B	�2B	��B	�"B	�B	�(B	�(B	�(B	�BB	�HB
 4B
 4B
 B	�HB	�]B
 4B
GB
MB
SB
MB
uB
gB
YB
YB
{B
tB
fB
	lB
	lB
	lB

XB
^B

rB
	�B
xB
jB
pB
�B
pB
�B
{B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
B
B
�B
�B
�B
�B
 �B
 �B
 �B
�B
B
!B
5B
!B
!B
!�B
!�B
 �B
 B
!B
!�B
"�B
"4B
"4B
&B
%�B
&B
&2B
'8B
*B
*B
)B
*0B
)B
)*B
(>B
)*B
)*B
'RB
*0B
+6B
+B
*0B
+6B
*KB
*KB
)DB
(>B
*KB
-)B
-)B
-)B
./B
-]B
/OB
/OB
.cB
.IB
0UB
1AB
1[B
1[B
2aB
3MB
3MB
2aB
2aB
1vB
0oB
3hB
6FB
6`B
5tB
6`B
7fB
7�B
7�B
8RB
8�B
8lB
7�B
5�B
5�B
4�B
9�B
;�B
<�B
<jB
<�B
<�B
<�B
<�B
;�B
9�B
9�B
=�B
=�B
=�B
<�B
>�B
=�B
=�B
=�B
<�B
>�B
>�B
>�B
>�B
=�B
=�B
=�B
=�B
<�B
>�B
>�B
?�B
>�B
>�B
@�B
@�B
A�B
A�B
@�B
?�B
@�B
B�B
B�B
C�B
C�B
B�B
A�B
@�B
B�B
E�B
E�B
E�B
E�B
E�B
E�B
D�B
D�B
D�B
E�B
C�B
D�B
C�B
E�B
F�B
F�B
E�B
E�B
F�B
G�B
I�B
H�B
H�B
G�B
I�B
I�B
H�B
G�B
G�B
IB
I�B
I�B
J�B
J�B
J�B
J�B
J�B
K�B
L�B
L�B
L�B
L�B
L�B
L�B
MB
K�B
MB
MB
M�B
NB
MB
MB
M�B
NB
L�B
NB
N�B
M�B
NB
NB
M�B
L0B
LB
N�B
PB
OB
OB
Q B
RB
Q�B
RB
QB
PB
O�B
PB
Q B
QB
P.B
Q4B
Q4B
T,B
T,B
T,B
T,B
S@B
S@B
U2B
V9B
W?B
X+B
XB
X+B
XB
W?B
W$B
XB
X+B
W?B
VSB
UMB
XEB
Y1B
YKB
Z7B
Z7B
Y1B
ZQB
Z7B
Y1B
YeB
ZQB
ZQB
ZkB
ZQB
\]B
]IB
]IB
]IB
\]B
[WB
[WB
[WB
[qB
]IB
]dB
]dB
]dB
]dB
^jB
^jB
_VB
_;B
_VB
_;B
_VB
_VB
_VB
^jB
^jB
]~B
_pB
^jB
^jB
^�B
_pB
`vB
a|B
`vB
`\B
`vB
_�B
abB
abB
`�B
b�B
b�B
a|B
`vB
`�B
b�B
dtB
ezB
ezB
dtB
d�B
d�B
c�B
b�B
c�B
e�B
e�B
ezB
d�B
e�B
h�B
hsB
hsB
gmB
gmB
e�B
e�B
f�B
f�B
g�B
h�B
h�B
g�B
g�B
g�B
h�B
h�B
i�B
i�B
i�B
i�B
i�B
i�B
i�B
i�B
j�B
j�B
j�B
j�B
j�B
i�B
j�B
i�B
i�B
k�B
k�B
k�B
k�B
k�B
l�B
l�B
l�B
k�B
j�B
l�B
m�B
m�B
m�B
l�B
l�B
m�B
m�B
m�B
m�B
n�B
n�B
m�B
n�B
o�B
o�B
n�B
o�B
o�B
n�B
o�B
o�B
o�B
p�B
p�B
p�B
r�B
r�B
r�B
q�B
r�B
r�B
r�B
r�B
q�B
q�B
p�B
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
t�B
t�B
u�B
t�B
t�B
t�B
s�B
tB
s�B
t�B
t�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   @��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
PRES            TEMP            PSAL            PRES            TEMP            PSAL            PRES            TEMP            PSAL            PRES_ADJ=PRES-SP,  where SP is SURFACE PRESSURE from next cycle; PRES_ADJ_ERR=Manufacturer sensor accuracy                                                                                                                                                      TEMP_ADJ=TEMP; TEMP_ADJ_ERR=Manufacturer sensor accuracy                                                                                                                                                                                                        PSAL_ADJ = RecalS= psal(PRES_ADJ,TEMP,Conductivity)                                                                                                                                                                                                             None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            PSAL_ADJ = celltm_sbe41(RecalS,TEMP,P,elapsed_time,alpha,tau); elapsed_time=P/mean_rise_rate; P=dbar since the start of the profile for each samples                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            PSAL_ADJ_ERR=max(RecalS & CTM error, 0.01(PSS-78))                                                                                                                                                                                                              SP=0.15(dbar)                                                                                                                                                                                                                                                   None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            alpha=0.0267C, tau=18.6s, mean_rise_rate = 0.09 dbar/second                                                                                                                                                                                                     None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            Pressure Correction using reported SURFACE PRESSURE                                                                                                                                                                                                             None                                                                                                                                                                                                                                                            Salinity Recalculation using PRES_ADJ                                                                                                                                                                                                                           None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            Correction for conductivity cell thermal mass error(CTM), Johnson et al., 2007, JAOT                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            No adjustment is needed                                                                                                                                                                                                                                         201809120038592018091200385920180912003859201809120200232018091202002320180912020023201809130030052018091300300520180913003005  JA  ARFMdecpA19c                                                                20180908093513  IP                  G�O�G�O�G�O�                JA  ARFMfmtp2.8                                                                 20180908003523  IP                  G�O�G�O�G�O�                JA  ARCAsspa3.1b                                                                20180908003527  IP  PRES            G�O�G�O�G�O�                JA  ARGQrqcppo_c                                                                20180908003527  QCP$                G�O�G�O�G�O�              3CJA  ARGQrqcpt19d                                                                20180908003528  QCP$                G�O�G�O�G�O�           80000JA  ARGQaqcpt19d                                                                20180908003528  QCP$                G�O�G�O�G�O�           80000JA  ARGQrqcp2.8e                                                                20180908003528  QCP$                G�O�G�O�G�O�            FB40JA  ARGQaqcp2.8e                                                                20180908003528  QCP$                G�O�G�O�G�O�            FB40JA  ARGQrqcpt16c                                                                20180908003528  QCP$                G�O�G�O�G�O�           10000JA      jafc1.0                                                                 20180908003528                      G�O�G�O�G�O�                JA  ARUP                                                                        20180908005617                      G�O�G�O�G�O�                JM  ARGQJMQC2.0                                                                 20180908153439  CV  JULD            G�O�G�O�F���                JM  ARCAJMQC2.0                                                                 20180911153859  CV  PRES_ADJUSTED   G�O�G�O�G�O�                JM  ARCAJMQC2.0                                                                 20180911153859  CV  PSAL_ADJUSTED   G�O�G�O�G�O�                JM  ARCAJMTM1.0                                                                 20180911170023  CV  PSAL_ADJUSTED   G�O�G�O�G�O�                JM  ARSQOW  1.1 2017V1                                                          20180912153005  IP  PSAL_ADJUSTED   G�O�G�O�G�O�                JA  ARDU                                                                        20200116231516                      G�O�G�O�G�O�                