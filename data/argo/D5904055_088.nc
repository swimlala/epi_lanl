CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       d2016-07-26T18:01:56Z creation; 2016-07-26T18:01:56Z updated; 2016-09-02T17:52:22Z converted from 3.0   
references        (http://www.argodatamgt.org/Documentation   comment           user_manual_version       3.1    Conventions       Argo-3.1 CF-1.6    featureType       trajectoryProfile         @   	DATA_TYPE                  	long_name         	Data type      conventions       Argo reference table 1     
_FillValue                    6�   FORMAT_VERSION                 	long_name         File format version    
_FillValue                    6�   HANDBOOK_VERSION               	long_name         Data handbook version      
_FillValue                    6�   REFERENCE_DATE_TIME                 	long_name         !Date of reference for Julian days      conventions       YYYYMMDDHHMISS     
_FillValue                    6�   DATE_CREATION                   	long_name         Date of file creation      conventions       YYYYMMDDHHMISS     
_FillValue                    6�   DATE_UPDATE                 	long_name         Date of update of this file    conventions       YYYYMMDDHHMISS     
_FillValue                    7   PLATFORM_NUMBER                   	long_name         Float unique identifier    conventions       WMO float identifier : A9IIIII     
_FillValue                    7   PROJECT_NAME                  	long_name         Name of the project    
_FillValue                  @  7$   PI_NAME                   	long_name         "Name of the principal investigator     
_FillValue                  @  7d   STATION_PARAMETERS           	            	long_name         ,List of available parameters for the station   conventions       Argo reference table 3     
_FillValue                  0  7�   CYCLE_NUMBER               	long_name         Float cycle number     conventions       =0...N, 0 : launch cycle (if exists), 1 : first complete cycle      
_FillValue         ��        7�   	DIRECTION                  	long_name         !Direction of the station profiles      conventions       -A: ascending profiles, D: descending profiles      
_FillValue                    7�   DATA_CENTRE                   	long_name         .Data centre in charge of float data processing     conventions       Argo reference table 4     
_FillValue                    7�   DC_REFERENCE                  	long_name         (Station unique identifier in data centre   conventions       Data centre convention     
_FillValue                     7�   DATA_STATE_INDICATOR                  	long_name         1Degree of processing the data have passed through      conventions       Argo reference table 6     
_FillValue                    8    	DATA_MODE                  	long_name         Delayed mode or real time data     conventions       >R : real time; D : delayed mode; A : real time with adjustment     
_FillValue                    8   PLATFORM_TYPE                     	long_name         Type of float      conventions       Argo reference table 23    
_FillValue                     8   FLOAT_SERIAL_NO                   	long_name         Serial number of the float     
_FillValue                     8(   FIRMWARE_VERSION                  	long_name         Instrument firmware version    
_FillValue                     8H   WMO_INST_TYPE                     	long_name         Coded instrument type      conventions       Argo reference table 8     
_FillValue                    8h   JULD               	long_name         ?Julian day (UTC) of the station relative to REFERENCE_DATE_TIME    standard_name         time   units         "days since 1950-01-01 00:00:00 UTC     conventions       8Relative julian days with decimal part (as parts of day)   
resolution        >�E�vQ�   
_FillValue        A.�~       axis      T           8l   JULD_QC                	long_name         Quality on date and time   conventions       Argo reference table 2     
_FillValue                    8t   JULD_LOCATION                  	long_name         @Julian day (UTC) of the location relative to REFERENCE_DATE_TIME   units         "days since 1950-01-01 00:00:00 UTC     conventions       8Relative julian days with decimal part (as parts of day)   
resolution        >�E�vQ�   
_FillValue        A.�~            8x   LATITUDE               	long_name         &Latitude of the station, best estimate     standard_name         latitude   units         degree_north   
_FillValue        @�i�       	valid_min         �V�        	valid_max         @V�        axis      Y           8�   	LONGITUDE                  	long_name         'Longitude of the station, best estimate    standard_name         	longitude      units         degree_east    
_FillValue        @�i�       	valid_min         �f�        	valid_max         @f�        axis      X           8�   POSITION_QC                	long_name         ,Quality on position (latitude and longitude)   conventions       Argo reference table 2     
_FillValue                    8�   POSITIONING_SYSTEM                    	long_name         Positioning system     
_FillValue                    8�   VERTICAL_SAMPLING_SCHEME                  	long_name         Vertical sampling scheme   conventions       Argo reference table 16    
_FillValue                    8�   CONFIG_MISSION_NUMBER                  	long_name         :Unique number denoting the missions performed by the float     conventions       !1...N, 1 : first complete mission      
_FillValue         ��        9�   PROFILE_PRES_QC                	long_name         #Global quality flag of PRES profile    conventions       Argo reference table 2a    
_FillValue                    9�   PROFILE_TEMP_QC                	long_name         #Global quality flag of TEMP profile    conventions       Argo reference table 2a    
_FillValue                    9�   PROFILE_PSAL_QC                	long_name         #Global quality flag of PSAL profile    conventions       Argo reference table 2a    
_FillValue                    9�   PRES         
      
   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���   axis      Z        L  9�   PRES_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  H�   PRES_ADJUSTED            
      	   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     L  L�   PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  \   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         decibar    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     L  _�   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     L  o8   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ~�   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     L  �X   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ��   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         degree_Celsius     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     L  �x   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     L  ��   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     L  ��   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �0   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     L  �   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  0  �P   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                    ڀ   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                    ݀   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                    ��   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  ,  �   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                    �   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                    �   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                    �   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                    �   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  @  �   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                    ��   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                    �   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    �   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar         �    HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar         �$   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�        �(   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    �,  �,Argo profile    3.1 1.2 19500101000000  20160726180156  20181103100343  5904055 US ARGO PROJECT                                                 GREGORY C. JOHNSON                                              PRES            TEMP            PSAL               XA   AO  5004                            2C  D   NAVIS_A                         0303                            082713                          863 @׾[�B71   @׾\��L�@75?|�h�dőhr�!1   GPS     Primary sampling: averaged [1Hz sampling by SBE-41CP averaged in 2-dbar bins]                                                                                                                                                                                      XA   A   A   @�  @�  A   A   A@  A`  A�  A�  A�  A�  A�  A�  A�  A�  B   B  B  B  B   B'��B0  B8  B@  BH  BP  BX  B`  Bh  Bp  Bx  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�33B�33B�33B�  B�  B�  B�  C   C  C  C  C  C
  C  C  C  C  C  C  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  Dy�D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dt� Du  Du� Dv  Dv� Dw  Dw� Dx  Dx� Dy  Dy� Dz  Dz� D{  D{� D|  D|� D}  D}� D~  D~� D  D� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D D�� D�  D�@ DÀ D�� D�  D�@ DĀ D�� D�  D�@ Dŀ D�� D�  D�@ Dƀ D�� D�  D�@ Dǀ D�� D�  D�@ DȀ D�� D�  D�@ Dɀ D�� D�  D�@ Dʀ D�� D�  D�@ Dˀ D�� D�  D�@ D̀ D�� D�  D�@ D̀ D�� D�  D�@ D΀ D�� D�  D�@ Dπ D�� D�  D�@ DЀ D�� D�  D�@ Dр D�� D�  D�@ DҀ D�� D�  D�@ DӀ D�� D�  D�@ DԀ D�� D�  D�@ DՀ D�� D�  D�@ Dր D�� D�  D�@ D׀ D�� D�  D�@ D؀ D�� D�  D�@ Dـ D�� D�  D�@ Dڀ D�� D�  D�@ Dۀ D�� D�  D�@ D܀ D�� D�  D�@ D݀ D�� D�  D�@ Dހ D�� D�  D�@ D߀ D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�3D��3D�� 1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 @vff@�33@�33A��A=��A]��A}��A���A���A���A���A���A���A���A���BffBffBffBffB'  B/ffB7ffB?ffBGffBOffBWffB_ffBgffBoffBwffBffB��3B��3B��3B��3B��3B��3B��3B��3B��3B��3B��3B��3B��3B��3B��3B��3Bó3Bǳ3B˳3Bϳ3Bӳ3B׳3B۳3B߳3B��fB��fB��fB�3B�3B��3B��3B��3CٚCٚCٚCٚC	ٚCٚCٚCٚCٚCٚCٚCٚCٚCٚCٚCٚC!ٚC#ٚC%ٚC'ٚC)ٚC+ٚC-ٚC/ٚC1ٚC3ٚC5ٚC7ٚC9ٚC;ٚC=ٚC?ٚCAٚCCٚCEٚCGٚCIٚCKٚCMٚCOٚCQٚCSٚCUٚCWٚCYٚC[ٚC]ٚC_ٚCaٚCcٚCeٚCgٚCiٚCkٚCmٚCoٚCqٚCsٚCuٚCwٚCyٚC{ٚC}ٚCٚC���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���D vfD �fDvfD�fDvfD�fDvfD�fDvfD�fDvfD�fDvfD�fDvfD�fDvfD�fD	vfD	�fD
vfD
�fDvfD�fDvfD�fDvfD�fDvfD�fDvfD�fDvfD�fDvfD�fDvfD�fDvfD�fDp D�fDvfD�fDvfD�fDvfD�fDvfD�fDvfD�fDvfD�fDvfD�fDvfD�fDvfD�fDvfD�fDvfD�fD vfD �fD!vfD!�fD"vfD"�fD#vfD#�fD$vfD$�fD%vfD%�fD&vfD&�fD'vfD'�fD(vfD(�fD)vfD)�fD*vfD*�fD+vfD+�fD,vfD,�fD-vfD-�fD.vfD.�fD/vfD/�fD0vfD0�fD1vfD1�fD2vfD2�fD3vfD3�fD4vfD4�fD5vfD5�fD6vfD6�fD7vfD7�fD8vfD8�fD9vfD9�fD:vfD:�fD;vfD;�fD<vfD<�fD=vfD=�fD>vfD>�fD?vfD?�fD@vfD@�fDAvfDA�fDBvfDB�fDCvfDC�fDDvfDD�fDEvfDE�fDFvfDF�fDGvfDG�fDHvfDH�fDIvfDI�fDJvfDJ�fDKvfDK�fDLvfDL�fDMvfDM�fDNvfDN�fDOvfDO�fDPvfDP�fDQvfDQ�fDRvfDR�fDSvfDS�fDTvfDT�fDUvfDU�fDVvfDV�fDWvfDW�fDXvfDX�fDYvfDY�fDZvfDZ�fD[vfD[�fD\vfD\�fD]vfD]�fD^vfD^�fD_vfD_�fD`vfD`�fDavfDa�fDbvfDb�fDcvfDc�fDdvfDd�fDevfDe�fDfvfDf�fDgvfDg�fDhvfDh�fDivfDi�fDjvfDj�fDkvfDk�fDlvfDl�fDmvfDm�fDnvfDn�fDovfDo�fDpvfDp�fDqvfDq�fDrvfDr�fDsvfDs�fDtvfDt�fDuvfDu�fDvvfDv�fDwvfDw�fDxvfDx�fDyvfDy�fDzvfDz�fD{vfD{�fD|vfD|�fD}vfD}�fD~vfD~�fDvfD�fD�;3D�{3D��3D��3D�;3D�{3D��3D��3D�;3D�{3D��3D��3D�;3D�{3D��3D��3D�;3D�{3D��3D��3D�;3D�{3D��3D��3D�;3D�{3D��3D��3D�;3D�{3D��3D��3D�;3D�{3D��3D��3D�;3D�{3D��3D��3D�;3D�{3D��3D��3D�;3D�{3D��3D��3D�;3D�{3D��3D��3D�;3D�{3D��3D��3D�;3D�{3D��3D��3D�;3D�{3D��3D��3D�;3D�{3D��3D��3D�;3D�{3D��3D��3D�;3D�{3D��3D��3D�;3D�{3D��3D��3D�;3D�{3D��3D��3D�;3D�{3D��3D��3D�;3D�{3D��3D��3D�;3D�{3D��3D��3D�;3D�{3D��3D��3D�;3D�{3D��3D��3D�;3D�{3D��3D��3D�;3D�{3D��3D��3D�;3D�{3D��3D��3D�;3D�{3D��3D��3D�;3D�{3D��3D��3D�;3D�{3D��3D��3D�;3D�{3D��3D��3D�;3D�{3D��3D��3D�;3D�{3D��3D��3D�;3D�{3D��3D��3D�;3D�{3D��3D��3D�;3D�{3D��3D��3D�;3D�{3D��3D��3D�;3D�{3D��3D��3D�;3D�{3D��3D��3D�;3D�{3D��3D��3D�;3D�{3D��3D��3D�;3D�{3D��3D��3D�;3D�{3D��3D��3D�;3D�{3D��3D��3D�;3D�{3D��3D��3D�;3D�{3D��3D��3D�;3D�{3D��3D��3D�;3D�{3D��3D��3D�;3D�{3D��3D��3D�;3D�{3D��3D��3D�;3D�{3D��3D��3D�;3D�{3D��3D��3D�;3D�{3D��3D��3D�;3D�{3D��3D��3D�;3D�{3D��3D��3D�;3D�{3D��3D��3D�;3D�{3D��3D��3D�;3D�{3D��3D��3D�;3D�{3D��3D��3D�;3D�{3D��3D��3D�;3D�{3D��3D��3D�;3D�{3D��3D��3D�;3D�{3D��3D��3D�;3D�{3D��3D��3D�;3D�{3D»3D��3D�;3D�{3Dû3D��3D�;3D�{3DĻ3D��3D�;3D�{3DŻ3D��3D�;3D�{3Dƻ3D��3D�;3D�{3Dǻ3D��3D�;3D�{3DȻ3D��3D�;3D�{3Dɻ3D��3D�;3D�{3Dʻ3D��3D�;3D�{3D˻3D��3D�;3D�{3D̻3D��3D�;3D�{3Dͻ3D��3D�;3D�{3Dλ3D��3D�;3D�{3Dϻ3D��3D�;3D�{3Dл3D��3D�;3D�{3Dѻ3D��3D�;3D�{3Dһ3D��3D�;3D�{3Dӻ3D��3D�;3D�{3DԻ3D��3D�;3D�{3Dջ3D��3D�;3D�{3Dֻ3D��3D�;3D�{3D׻3D��3D�;3D�{3Dػ3D��3D�;3D�{3Dٻ3D��3D�;3D�{3Dڻ3D��3D�;3D�{3Dۻ3D��3D�;3D�{3Dܻ3D��3D�;3D�{3Dݻ3D��3D�;3D�{3D޻3D��3D�;3D�{3D߻3D��3D�;3D�{3D�3D��3D�;3D�{3D�3D��3D�;3D�{3D�3D��3D�;3D�{3D�3D��3D�;3D�{3D�3D��3D�;3D�{3D�3D��3D�;3D�{3D�3D��3D�;3D�{3D�3D��3D�;3D�{3D�3D��3D�;3D�{3D�3D��3D�;3D�{3D�3D��3D�;3D�{3D�3D��3D�;3D�{3D�3D��3D�;3D�{3D��3D��3D�;3D�{3D�3D��3D�;3D�{3D�3D��3D�;3D�{3D�3D��3D�;3D�{3D�3D��3D�;3D�{3D�3D��3D�;3D�{3D�3D��3D�;3D�~fD��fD��31111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 @��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��Aӛ�Aӥ�Aӥ�Aӟ�Aӛ�Aә�Aӕ�AӋDA�z�A�t�A�1'A҉7Aѥ�AоwA�C�A�K�A��A�E�A���A�E�A���A�XA�jA��#A�/A���A�"�A���A��A��A�ƨA��A�C�A��A�VA�-A���A�1'A�bA��A�1'A�^5A��
A�A�\)A���A�&�A�z�A� �A��A�t�A�^5A�JA�9XA���A���A��\A�5?A�hsA��/A��FA��DA�t�A��jA�M�A�jA�hsA�Q�A���A�$�A���A�^5A�|�A��hA��wA�ffA��9A�JA��PA�33A�"�A���A�XA���A�&�A�ȴA�|�A�XA�bA�hsA�bNA�A��#A��jA��RA��RA��A���A�t�A�?}A��A�^A~��A|v�Az��AzE�Ay�-Ayp�Ax�`AxffAw�AwG�Av�\Au�-AuAt{As�Arr�Ap��Ao�An �Am`BAl~�Ak�^Ah��Ag��Afr�Ael�AeoAdZAb�DA^v�A^�A]�A]��A]dZAZ�9AZ-AZJAY�PAXJAWhsAV�AVbAUK�AT^5AT(�AS��ARbNAR�AQ7LAO�;AM�AM+ALffAK��AJ��AJ��AJz�AJ �AI��AI|�AIK�AHQ�AF1'AD�HACAB�/AB-AA��AA�wA@A�A=dZA;��A:  A7��A6��A6  A5�A3\)A21'A1�7A0�yA/C�A.��A.M�A-t�A, �A+S�A*�/A)�mA)�PA(��A't�A'VA%K�A#|�A"�jA!��A!�A �\A��A��A�A7LA�`A�DAVAl�AA&�A  AK�A�`Av�A�7AȴA�9A�A�DAv�AZA�A��A$�A�A�-A��Al�AbA`BA�A��A�/A��A��Ax�A�A�
A
�/A	?}A�9A��Av�AjA^5AI�A �AJA�TA��A��A�yA��A�9A �AV@��@��F@�%@�v�@��@�n�@�Q�@�E�@�
=@���@��@��@�;d@��H@�M�@��y@���@��@��@��#@Ӆ@��@҇+@�-@��@�@ѡ�@�&�@�bN@���@���@�dZ@�ff@ͺ^@�hs@�O�@�%@̋D@��@�;d@�ff@ɡ�@�(�@š�@Ĭ@�9X@�b@��
@�ff@��@�bN@��@��P@���@�^5@�hs@��m@�|�@���@�@��@��#@���@���@���@���@��h@�O�@�/@��j@�(�@��@��@��P@���@���@�G�@��@�ȴ@���@��\@�ff@��@��7@�X@�?}@��@��m@�C�@�ff@�J@��-@�?}@�7L@�/@��@�|�@��H@���@��9@�r�@�r�@�r�@�bN@�1'@�1@��
@�t�@�"�@��!@�O�@��@�r�@�Z@�Z@�Q�@��@��w@�33@���@���@�=q@���@�p�@��`@���@���@�1'@��P@�dZ@�l�@�K�@�33@�
=@�v�@�E�@��@��T@��#@��@��^@��@���@�I�@�1'@���@�l�@�@�v�@���@�O�@�\)@�J@���@�^5@��@�G�@���@���@��@��@�Ĝ@��;@�|�@�v�@�5?@�@��h@�hs@���@�Q�@�9X@���@�
=@�ȴ@�$�@��#@��-@���@���@��h@�p�@�G�@�%@��/@��j@�bN@�|�@�"�@�
=@��@���@��R@��!@���@��+@�5?@�J@��@���@��7@��7@��7@�x�@�X@��@���@��@��D@��u@��D@��D@�z�@�Z@�r�@�r�@�r�@�bN@�Q�@�A�@�1'@� �@�1@�;@�@�@�;@�1@�b@��@�1@��@\)@~�+@~�+@~�+@~�+@~�+@~v�@~ȴ@~�y@~��@~��@~��@~��@+@~�y@~�+@~ff@~$�@}@}`B@}?}@|�@|9X@{�@{"�@z��@z�!@z^5@yx�@x��@y7L@yX@y��@y�#@y�#@y��@y��@y�7@yhs@yX@y&�@x�`@xĜ@xbN@x  @w+@v5?@u`B@t�j@tz�@s�@r^5@r�@qhs@p�@o�w@o\)@o+@o+@o�@o�w@o��@o��@o��@oK�@o;d@o+@o�@n�y@n{@m?}@l�j@lZ@l9X@lZ@lI�@lZ@lZ@lZ@lZ@l(�@k�
@kt�@k33@k"�@k@j�H@j��@j��@jJ@i��@i�7@i��@i�7@ix�@ix�@i�7@i�7@ix�@h�`@h�u@hA�@h �@gK�@f��@d��@d�D@d9X@c�F@c��@ct�@cS�@b�@b��@b��@b��@b~�@b�@a�#@a�^@a��@a��@ahs@aG�@`�@`Q�@`A�@`1'@_�@_l�@_;d@^�y@^�R@^�R@^�R@^5?@]�T@]p�@\�@\Z@[�@Z~�@ZJ@Y�#@Y��@Yhs@YG�@Y�@X�9@X �@W�;@W�@W�P@W\)@V��@U�T@U`B@T��@T�@Tj@T(�@SS�@Rn�@Q�#@Q�@P�@Pr�@PQ�@P1'@Pb@O�;@OK�@Nȴ@Nv�@NV@NE�@NE�@NE�@N5?@N$�@N{@N{@N@M�T@M�-@M`B@L��@L��@L�D@LI�@K�F@KS�@J��@IG�@H��@H�@HA�@G�@G|�@G\)@GK�@G;d@G�@F��@F�y@F�@F�@F�@F�@F�R@F��@Fv�@FE�@F{@F@F@F@F@F@E�T@E?}@D�j@D�@C�@Bn�@A�#@A��@Ax�@AG�@A�@A%@@bN@?�@?K�@>��@>�R@>{@=�-@=�@=?}@=V@<��@<��@<Z@;��@;33@:�H@:��@:�!@:�\@:^5@:^5@:-@9�#@9��@9x�@9�@8Ĝ@8r�@7�w@7;d@6��@6��@6E�@5�@4��@4�/@4�j@4��@4j@4(�@4�@2=q@1�7@17L@0Ĝ@0�u@0Q�@0  @/�@/�P@/+@.ȴ@.ff@-@-/@,j@,�@+��@+��@+��@+dZ@+"�@*��@*��@*~�@*^5@*-@*J@)�#@)��@)hs@(�u@(b@'��@'��@'\)@'K�@';d@'�@&�y@&�R@&�+@&�+@&V@&@%�@%O�@%?}@$��@$�/@$�D@$Z@$(�@#�m@#�
@#�F@#�@#S�@#"�@#@"�@"��@"M�@!��@!�^@!x�@!�@ �9@ Q�@�@��@�@l�@��@ȴ@��@��@�+@E�@{@�T@�h@�@p�@O�@/@/@/@V@�@�/@�@�@j@�
@33@�@��@�!@=q@��@G�@&�@�@�`@A�@  @�;@��@�@+@ȴ@�R@��@��@v�@V@E�@5?@$�@�@�@�T@�T@�T@�T@@`B@O�@?}@?}@?}@�@�j@��@�D@j@�@��@��@�m@C�@~�@=q@�@��@�@�@�#@�^@�^@��@G�@&�@��@��@�u@Q�@1'@ �@  @�;@�@�P@K�@
=@��@E�@5?@@O�@O�@?}@?}@/@�@��@Z@Z@Z@Z@Z@Z@I�@(�@�m@��@33@o@@
�H@
��@
��@
�\@
n�@
^5@
M�@
M�@
=q@
=q@
=q@
=q@
�@	�@	�^@	��@	�7@	�7@	x�@	hs@	G�@	7L@	&�@	�@	�@	%@	%@��@��1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 Aӛ�Aӥ�Aӥ�Aӟ�Aӛ�Aә�Aӕ�AӋDA�z�A�t�A�1'A҉7Aѥ�AоwA�C�A�K�A��A�E�A���A�E�A���A�XA�jA��#A�/A���A�"�A���A��A��A�ƨA��A�C�A��A�VA�-A���A�1'A�bA��A�1'A�^5A��
A�A�\)A���A�&�A�z�A� �A��A�t�A�^5A�JA�9XA���A���A��\A�5?A�hsA��/A��FA��DA�t�A��jA�M�A�jA�hsA�Q�A���A�$�A���A�^5A�|�A��hA��wA�ffA��9A�JA��PA�33A�"�A���A�XA���A�&�A�ȴA�|�A�XA�bA�hsA�bNA�A��#A��jA��RA��RA��A���A�t�A�?}A��A�^A~��A|v�Az��AzE�Ay�-Ayp�Ax�`AxffAw�AwG�Av�\Au�-AuAt{As�Arr�Ap��Ao�An �Am`BAl~�Ak�^Ah��Ag��Afr�Ael�AeoAdZAb�DA^v�A^�A]�A]��A]dZAZ�9AZ-AZJAY�PAXJAWhsAV�AVbAUK�AT^5AT(�AS��ARbNAR�AQ7LAO�;AM�AM+ALffAK��AJ��AJ��AJz�AJ �AI��AI|�AIK�AHQ�AF1'AD�HACAB�/AB-AA��AA�wA@A�A=dZA;��A:  A7��A6��A6  A5�A3\)A21'A1�7A0�yA/C�A.��A.M�A-t�A, �A+S�A*�/A)�mA)�PA(��A't�A'VA%K�A#|�A"�jA!��A!�A �\A��A��A�A7LA�`A�DAVAl�AA&�A  AK�A�`Av�A�7AȴA�9A�A�DAv�AZA�A��A$�A�A�-A��Al�AbA`BA�A��A�/A��A��Ax�A�A�
A
�/A	?}A�9A��Av�AjA^5AI�A �AJA�TA��A��A�yA��A�9A �AV@��@��F@�%@�v�@��@�n�@�Q�@�E�@�
=@���@��@��@�;d@��H@�M�@��y@���@��@��@��#@Ӆ@��@҇+@�-@��@�@ѡ�@�&�@�bN@���@���@�dZ@�ff@ͺ^@�hs@�O�@�%@̋D@��@�;d@�ff@ɡ�@�(�@š�@Ĭ@�9X@�b@��
@�ff@��@�bN@��@��P@���@�^5@�hs@��m@�|�@���@�@��@��#@���@���@���@���@��h@�O�@�/@��j@�(�@��@��@��P@���@���@�G�@��@�ȴ@���@��\@�ff@��@��7@�X@�?}@��@��m@�C�@�ff@�J@��-@�?}@�7L@�/@��@�|�@��H@���@��9@�r�@�r�@�r�@�bN@�1'@�1@��
@�t�@�"�@��!@�O�@��@�r�@�Z@�Z@�Q�@��@��w@�33@���@���@�=q@���@�p�@��`@���@���@�1'@��P@�dZ@�l�@�K�@�33@�
=@�v�@�E�@��@��T@��#@��@��^@��@���@�I�@�1'@���@�l�@�@�v�@���@�O�@�\)@�J@���@�^5@��@�G�@���@���@��@��@�Ĝ@��;@�|�@�v�@�5?@�@��h@�hs@���@�Q�@�9X@���@�
=@�ȴ@�$�@��#@��-@���@���@��h@�p�@�G�@�%@��/@��j@�bN@�|�@�"�@�
=@��@���@��R@��!@���@��+@�5?@�J@��@���@��7@��7@��7@�x�@�X@��@���@��@��D@��u@��D@��D@�z�@�Z@�r�@�r�@�r�@�bN@�Q�@�A�@�1'@� �@�1@�;@�@�@�;@�1@�b@��@�1@��@\)@~�+@~�+@~�+@~�+@~�+@~v�@~ȴ@~�y@~��@~��@~��@~��@+@~�y@~�+@~ff@~$�@}@}`B@}?}@|�@|9X@{�@{"�@z��@z�!@z^5@yx�@x��@y7L@yX@y��@y�#@y�#@y��@y��@y�7@yhs@yX@y&�@x�`@xĜ@xbN@x  @w+@v5?@u`B@t�j@tz�@s�@r^5@r�@qhs@p�@o�w@o\)@o+@o+@o�@o�w@o��@o��@o��@oK�@o;d@o+@o�@n�y@n{@m?}@l�j@lZ@l9X@lZ@lI�@lZ@lZ@lZ@lZ@l(�@k�
@kt�@k33@k"�@k@j�H@j��@j��@jJ@i��@i�7@i��@i�7@ix�@ix�@i�7@i�7@ix�@h�`@h�u@hA�@h �@gK�@f��@d��@d�D@d9X@c�F@c��@ct�@cS�@b�@b��@b��@b��@b~�@b�@a�#@a�^@a��@a��@ahs@aG�@`�@`Q�@`A�@`1'@_�@_l�@_;d@^�y@^�R@^�R@^�R@^5?@]�T@]p�@\�@\Z@[�@Z~�@ZJ@Y�#@Y��@Yhs@YG�@Y�@X�9@X �@W�;@W�@W�P@W\)@V��@U�T@U`B@T��@T�@Tj@T(�@SS�@Rn�@Q�#@Q�@P�@Pr�@PQ�@P1'@Pb@O�;@OK�@Nȴ@Nv�@NV@NE�@NE�@NE�@N5?@N$�@N{@N{@N@M�T@M�-@M`B@L��@L��@L�D@LI�@K�F@KS�@J��@IG�@H��@H�@HA�@G�@G|�@G\)@GK�@G;d@G�@F��@F�y@F�@F�@F�@F�@F�R@F��@Fv�@FE�@F{@F@F@F@F@F@E�T@E?}@D�j@D�@C�@Bn�@A�#@A��@Ax�@AG�@A�@A%@@bN@?�@?K�@>��@>�R@>{@=�-@=�@=?}@=V@<��@<��@<Z@;��@;33@:�H@:��@:�!@:�\@:^5@:^5@:-@9�#@9��@9x�@9�@8Ĝ@8r�@7�w@7;d@6��@6��@6E�@5�@4��@4�/@4�j@4��@4j@4(�@4�@2=q@1�7@17L@0Ĝ@0�u@0Q�@0  @/�@/�P@/+@.ȴ@.ff@-@-/@,j@,�@+��@+��@+��@+dZ@+"�@*��@*��@*~�@*^5@*-@*J@)�#@)��@)hs@(�u@(b@'��@'��@'\)@'K�@';d@'�@&�y@&�R@&�+@&�+@&V@&@%�@%O�@%?}@$��@$�/@$�D@$Z@$(�@#�m@#�
@#�F@#�@#S�@#"�@#@"�@"��@"M�@!��@!�^@!x�@!�@ �9@ Q�@�@��@�@l�@��@ȴ@��@��@�+@E�@{@�T@�h@�@p�@O�@/@/@/@V@�@�/@�@�@j@�
@33@�@��@�!@=q@��@G�@&�@�@�`@A�@  @�;@��@�@+@ȴ@�R@��@��@v�@V@E�@5?@$�@�@�@�T@�T@�T@�T@@`B@O�@?}@?}@?}@�@�j@��@�D@j@�@��@��@�m@C�@~�@=q@�@��@�@�@�#@�^@�^@��@G�@&�@��@��@�u@Q�@1'@ �@  @�;@�@�P@K�@
=@��@E�@5?@@O�@O�@?}@?}@/@�@��@Z@Z@Z@Z@Z@Z@I�@(�@�m@��@33@o@@
�H@
��@
��@
�\@
n�@
^5@
M�@
M�@
=q@
=q@
=q@
=q@
�@	�@	�^@	��@	�7@	�7@	x�@	hs@	G�@	7L@	&�@	�@	�@	%@	%@��@��1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 ;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oB�XB�RB�RB�RB�XB�RB�XB�XB�XB�RB�RB�RB�FB�XB��BĜB��B�/B��BDBJBJB\B�B&�B'�B)�B,B,B-B,B-B.B.B+B(�B+B33B49B@�BB�B=qB:^B2-B-B)�B#�B�B�BB�NBȴB�B��B��B�hB�DB�Bs�BiyBYBD�B:^B49B.B�BbB�B�#B��BɺB�LB��B��B�bB�=B� Bu�Bm�BffBT�B@�B)�B{B	7BB
��B
��B
��B
�B
�;B
�B
�B
�
B
�B
�B
��B
��B
��B
��B
ÖB
�RB
�B
��B
��B
�bB
�JB
�7B
�B
�B
}�B
x�B
s�B
m�B
iyB
cTB
^5B
W
B
M�B
E�B
:^B
5?B
/B
(�B
�B
VB
%B	��B	��B	��B	�mB	��B	��B	ɺB	ǮB	ÖB	�?B	�-B	�'B	�B	��B	��B	��B	��B	��B	��B	��B	�uB	�VB	�DB	�B	|�B	q�B	l�B	hsB	dZB	aHB	_;B	^5B	\)B	ZB	XB	VB	O�B	F�B	@�B	:^B	5?B	2-B	0!B	-B	#�B	�B	\B	1B	B��B��B��B�B�B�yB�fB�BB�5B�)B�B��B��B��B��BȴBĜB�}B�jB�LB�-B�B��B��B��B��B��B��B��B��B��B��B�hB�JB�1B�B�B�B� B}�B{�B{�B{�Bz�Bz�By�Bx�Bw�Bu�Bt�Bs�Br�Bq�Bo�Bm�Bl�Bl�Bk�BjBhsBdZBbNB_;B\)BZBYBYBYBYBYBXBXBXBW
BVBT�BR�BQ�BO�BM�BK�BH�BE�BC�BC�BA�BA�BA�B@�B@�B?}B=qB<jB:^B9XB8RB9XB<jB>wB>wB@�BC�BE�BF�BF�BG�BH�BH�BI�BL�BN�BN�BO�BS�BVBXBW
BXBYB[#B]/B_;B`BBe`Bp�Bu�By�By�Bz�B�B�=B�JB�PB�VB�bB�hB�oB��B��B��B��B��B��B��B��B��B��B��B�B�B�B�B�B�B�!B�-B�RB�^B��BĜBĜBĜBŢBǮBȴBɺBɺBɺB��B��B�B�B�#B�BB�NB�NB�TB�mB�fB�B�B��B��B��B��B	B	B	%B	
=B	PB	\B	�B	�B	�B	�B	�B	�B	 �B	!�B	%�B	(�B	)�B	)�B	,B	/B	2-B	49B	6FB	;dB	>wB	>wB	>wB	=qB	=qB	@�B	D�B	E�B	F�B	G�B	H�B	G�B	H�B	I�B	J�B	L�B	M�B	N�B	O�B	P�B	Q�B	R�B	W
B	W
B	T�B	XB	]/B	^5B	_;B	bNB	cTB	e`B	cTB	e`B	gmB	k�B	k�B	k�B	k�B	jB	k�B	n�B	x�B	{�B	{�B	z�B	z�B	|�B	}�B	� B	� B	�B	�B	�B	�B	�B	�B	�%B	�+B	�PB	�bB	�bB	�hB	�oB	�uB	�uB	�uB	�{B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�!B	�'B	�?B	�LB	�RB	�XB	�XB	�^B	�dB	�dB	�dB	�jB	�jB	�qB	�}B	��B	ĜB	ŢB	ƨB	ɺB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�
B	�B	�/B	�5B	�5B	�;B	�;B	�;B	�;B	�BB	�HB	�HB	�HB	�HB	�BB	�BB	�HB	�ZB	�fB	�sB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
  B
B
B
+B
1B
	7B
	7B

=B
DB
DB
DB
JB
JB
VB
\B
\B
\B
\B
bB
bB
bB
bB
bB
bB
bB
bB
bB
bB
bB
bB
hB
hB
hB
hB
uB
uB
uB
{B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
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
 �B
!�B
!�B
!�B
!�B
"�B
"�B
"�B
#�B
#�B
#�B
%�B
%�B
&�B
&�B
&�B
&�B
'�B
'�B
(�B
+B
+B
,B
,B
,B
,B
,B
-B
-B
.B
.B
.B
.B
.B
.B
.B
.B
.B
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
1'B
2-B
2-B
2-B
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
5?B
5?B
5?B
6FB
6FB
8RB
8RB
9XB
9XB
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
<jB
<jB
<jB
<jB
<jB
<jB
=qB
=qB
>wB
>wB
>wB
>wB
>wB
>wB
>wB
>wB
>wB
>wB
?}B
?}B
?}B
@�B
@�B
@�B
A�B
A�B
B�B
C�B
C�B
C�B
C�B
C�B
C�B
C�B
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
I�B
I�B
J�B
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
M�B
M�B
M�B
M�B
M�B
N�B
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
P�B
P�B
Q�B
Q�B
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
W
B
W
B
W
B
XB
XB
XB
XB
XB
YB
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
]/B
]/B
]/B
]/B
]/B
^5B
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
cTB
cTB
cTB
cTB
cTB
cTB
dZB
dZB
cTB
dZB
dZB
dZB
dZB
dZB
e`B
e`B
e`B
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
jB
jB
jB
jB
k�B
k�B
k�B
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
r�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 B�>B�dB�wB�tB�nB�tB��B��B��B��B�oB�B��B��B�bB�fBҢB��B#BB7B7B�B%eB)oB*5B,B-�B.(B/�B3YB6B2�B1%B/�B2�B5�B8B9BD*BFuB@-B>UB5uB0B-qB']B�B-BBB�3B��B�IB�B�wB�B�2B��BvBnbB]�BH�B=8B5�B1�B"�BSB�0B�8B�B�B� B��B�B��B�+B��Bw�Bo=BkB[xBE�B/�B
B
�B�B
��B
�GB
�B
�_B
�B
��B
سB
�-B
�B
�MB
�@B
��B
�B
��B
ǱB
�eB
�MB
��B
�]B
��B
�B
��B
�kB
�SB
�B
z�B
u�B
oeB
k�B
d�B
aB
Z�B
P�B
J�B
<�B
7�B
1�B
0�B
�B
�B
�B
 B	�B	�EB	��B	��B	�MB	�(B	�1B	�TB	��B	��B	��B	��B	��B	�KB	��B	��B	��B	�B	��B	�B	��B	�3B	�FB	�2B	sqB	nJB	jBB	e�B	b,B	_�B	_B	]TB	Z�B	X�B	X�B	UfB	I�B	C!B	<yB	6�B	2�B	0�B	0xB	)�B	NB	�B	�B	�B	PB��B��B�RB�&B�B�@B��B�B�>B�<B��B�#B�9B˹B��B�qB��B��B��B� B��B�3B�yB�*B�yB�yB�~B��B��B�2B��B�<B��B�6B�B�-B�mB��B�B|3B|B|[B{5B{UB{KB|*By�Bv�Bu~Bt%BscBu�Bq�BnfBl�Bl�Bk�Bk-Bk�Bf�Bd�Bb]B`�B[�BYnBY�BYOBYIBYdBX�BXcBX�BY�BY�BW(BVBBT�BQ�BP�BNzBL�BH�BFPBD�BD�BC�BC�BDBB�BB�B?�B=uB:�B:#B<nB>B>�B?�BATBC�BD�BF&BG,BGBG�BH�BIyBJ�BM|BO(BO�BQhBT�BV�BXJBW�BX�BZ"B\TB^�B`�Bb�BiMBr2Bv�Bz4BznB}<B��B�,B�B�B�_B��B�B��B�?B��B��B��B��B�B��B��B�nB�BB�ZB�=B��B��B�CB�dB��B�B�}B�B�^B��B��BľB��B�MB�LB�B��B�	B�JB��B�(B؛BڬB��B�`B�qB��B�_B�aB�(B��B� B��B��B�B�SB	^B	|B	�B	
�B	%B	~B	�B	
B	�B	�B	�B	,B	!rB	"�B	&�B	)QB	*�B	*�B	,�B	/�B	2[B	4gB	7JB	<tB	>�B	>B	>�B	=�B	=�B	AgB	D�B	E�B	GB	G�B	H�B	H
B	I&B	J�B	K�B	MB	N>B	O�B	P�B	Q�B	R�B	S�B	Y�B	X�B	UhB	WbB	]�B	_,B	_�B	bnB	d�B	h�B	eB	f�B	hB	mB	k�B	k�B	l5B	j�B	l>B	o�B	yB	|�B	|�B	{HB	{�B	}XB	~5B	�B	�(B	�$B	�AB	�RB	�vB	�_B	�ZB	��B	�[B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	��B	��B	��B	��B	��B	�&B	�UB	�B	�%B	�B	�'B	�/B	�NB	�iB	�+B	�`B	�hB	��B	��B	��B	��B	��B	��B	��B	��B	��B	�eB	�wB	ĤB	ŪB	��B	�	B	�8B	́B	��B	��B	��B	��B	��B	ѿB	��B	��B	�
B	�B	�B	��B	�XB	ڂB	�_B	�B	ޙB	ߜB	�kB	ߘB	��B	��B	�B	�B	�xB	�B	�B	��B	�B	�SB	�BB	�TB	�B	�B	��B	��B	��B	��B	��B	��B	��B	�B	�
B	�dB	�vB	�dB	�GB	�
B	�wB	��B	�B	�dB	��B	�~B	�KB
 .B
B
�B
5B
\B
	KB
	KB

�B
aB
bB
aB
�B
�B
 B
�B
�B
�B
ZB
�B
mB
vB
yB
yB
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
B
�B
�B
�B
MB
8B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
6B
�B
�B
�B
�B
B
�B
�B
�B
�B
�B
 !B
 B
 B
!+B
!>B
!^B
!vB
! B
 �B
 �B
!B
!�B
!�B
"!B
">B
#B
#B
"�B
$B
$2B
$�B
&QB
&:B
'6B
'0B
'4B
'�B
(�B
(rB
)�B
+}B
+ B
,3B
,3B
,1B
,AB
,�B
-�B
-XB
.?B
.4B
.)B
.)B
.4B
.4B
.4B
.)B
.2B
.?B
.MB
.cB
/�B
/PB
0FB
0fB
0�B
0�B
0�B
2.B
2zB
2�B
2rB
3�B
3hB
3`B
4YB
4WB
4eB
4aB
4VB
4VB
4KB
4KB
4LB
4dB
4dB
4dB
4oB
5sB
5_B
5UB
5SB
5UB
5TB
5mB
5�B
5�B
5�B
6�B
7B
8�B
8�B
9�B
9�B
9�B
9B
9�B
9�B
:�B
:�B
:�B
:�B
;�B
;�B
<�B
<�B
<�B
<�B
<�B
=	B
=�B
=�B
>�B
>�B
>�B
>�B
>�B
>�B
>�B
>�B
>�B
>�B
?�B
?�B
@B
@�B
@�B
@�B
A�B
B*B
CB
C�B
C�B
C�B
C�B
C�B
C�B
D�B
F<B
F�B
GB
F�B
F�B
HB
G�B
G�B
HB
IB
IB
JLB
J?B
KmB
LB
K�B
LB
K�B
K�B
LB
M B
L�B
L�B
L�B
MB
M�B
NB
NB
NB
NvB
O@B
PB
PB
PB
QB
QB
QB
QB
QB
QB
P�B
QB
Q2B
R[B
RB
SB
S4B
SB
S=B
S'B
T2B
T8B
TB
T#B
T0B
T,B
U7B
U*B
UB
U4B
UeB
VUB
VHB
VMB
V[B
WeB
WeB
WcB
X<B
X@B
XUB
XuB
XEB
YLB
Y)B
Y7B
Y^B
YMB
YOB
ZnB
Z=B
Z=B
ZMB
ZIB
[6B
[;B
[KB
[KB
[AB
[SB
[:B
[\B
[�B
\�B
]jB
]WB
]WB
]�B
]�B
^�B
_cB
_\B
_qB
_�B
`�B
`iB
`bB
`oB
`�B
a�B
aeB
blB
biB
bvB
bvB
bmB
bmB
boB
bB
chB
cuB
chB
chB
cjB
c�B
c�B
cvB
cqB
dnB
dpB
c�B
d�B
d�B
dxB
d�B
d�B
e�B
euB
e�B
d�B
e�B
f�B
f�B
g�B
g�B
g�B
g�B
g�B
g�B
g�B
g�B
h�B
h�B
h�B
h�B
h�B
i�B
i�B
i�B
i�B
i�B
j�B
j�B
j�B
j�B
k�B
k�B
k�B
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
r�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 <#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<k��<#�
<#�
<#�
<#�
<DW�<M�<#�
<#�
<#�
<#�
<#�
<#�
<#�
<%X]<#�
<#�
<#�
<3N�<?[�<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<I�c<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
PRES            TEMP            PSAL            PRES_ADJUSTED = PRES - (REPORTED_SURFACE_PRESSURE) for PADJ                                                                                                                                                                                                     none                                                                                                                                                                                                                                                            PSAL_ADJ corrects Cnd. Thermal Mass (CTM), Johnson et al,2007,JAOT & effects of pressure adjustments                                                                                                                                                            PADJ REPORTED_SURFACE_PRESSURE =0.15 dbar                                                                                                                                                                                                                       none                                                                                                                                                                                                                                                            CTM alpha = 0.141 & tau = 6.68 s with error equal to the correction                                                                                                                                                                                             Pressures adjusted using reported surface pressure when warranted.                                                                                                                                                                                              none                                                                                                                                                                                                                                                            Salinity corrected for pressure sensor calibration drift and cell thermal mass                                                                                                                                                                                  201609291650302016092916503020160929165030  AO  ARCAADJP                                                                    20160726180156    IP                G�O�G�O�G�O�                AO  ARGQQCPL                                                                    20160726180156  QCP$                G�O�G�O�G�O�5F03E           AO  ARGQQCPL                                                                    20160726180156  QCF$                G�O�G�O�G�O�0               PM  ARSQPADJV1.1                                                                20160929165030  QC  PRES            @�  D�� G�O�                PM  ARSQCTM V1.1                                                                20160929165030  QC  PSAL            @�  D�� G�O�                PM  ARSQOWGUV1.0                                                                20181103100343  IP                  G�O�G�O�G�O�                