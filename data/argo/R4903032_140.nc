CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       2022-05-14T18:35:58Z creation      
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
resolution        =���   axis      Z        h  9p   PRES_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  H�   PRES_ADJUSTED            
      	   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     h  L�   PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  \   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         decibar    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     h  _�   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     h  o`   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ~�   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     h  ��   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         degree_Celsius     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     h  ��   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     h  �P   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ��   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     h  ��   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ��   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     h  ��   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  0  �@   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                    �p   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                    �p   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                    �p   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  ,  �p   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                    �   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                    �   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                    �   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                    �   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  @  �   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                    ��   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                    ��   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    �    HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar         �   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar         �   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�        �   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    �Argo profile    3.1 1.2 19500101000000  20220514183558  20220514183558  4903032 Argo PMEL                                                       GREGORY C. JOHNSON                                              PRES            TEMP            PSAL               �A   AO  7212                            2B  A   NAVIS_A                         0942                            170425                          863 @���\�/1   @�����UF@;�bM���c��n��1   GPS     Primary sampling: averaged                                                                                                                                                                                                                                         �A   A   F   @�33@���A   AffA@  A`  A�  A�  A�  A�  A�  A�  A�  A�  B   B  B  B  B   B(  B0  B8  B@  BH  BP  BX  B`  Bh  Bp  Bw��B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  C   C  C  C  C  C
  C  C  C  C  C  C  C  C  C  C  C   C"  C$�C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  D   D � D  D� D  D�fD  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D fD �fD!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dt� Du  Du� Dv  Dv� Dw  Dw� Dx  Dx� Dy  Dy� Dz  Dz� D{  D{� D|  D|� D}  D}� D~  D~� D  D� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�<�D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�3D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�3D�@ D D�� D�  D�@ DÀ D�� D�  D�@ DĀ D�� D�  D�@ Dŀ D�� D�  D�@ Dƀ D�� D�  D�@ Dǀ D�� D�  D�@ DȀ D�� D�  D�@ Dɀ D�� D�  D�@ Dʀ D�� D�  D�@ Dˀ D�� D�  D�@ D̀ D�� D�  D�@ D̀ D�� D�  D�@ D΀ D�� D�  D�@ Dπ D�� D�  D�@ DЀ D�� D�  D�@ Dр D�� D�  D�@ DҀ D�� D�  D�@ DӀ D�� D�  D�@ DԀ D�� D�  D�@ DՀ D�� D�  D�@ Dր D�� D�  D�@ D׀ D�� D�  D�@ D؀ D�� D�  D�@ Dـ D�� D�  D�@ Dڀ D�� D�  D�@ Dۀ D�� D�  D�@ D܀ D�� D�  D�@ D݀ D�� D�  D�@ Dހ D�� D�  D�@ D߀ D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D��3D��311111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  @|��@�  @�33A  A=��A]��A}��A���A���A���A���A���A���A���A���BffBffBffBffB'ffB/ffB7ffB?ffBGffBOffBWffB_ffBgffBoffBw  BffB��3B��3B��3B��3B��3B��3B��3B��3B��3B��3B��3B��3B��3B��3B��3B��3Bó3Bǳ3B˳3Bϳ3Bӳ3B׳3B۳3B߳3B�3B�3B�3B�3B�3B��3B��3B��3CٚCٚCٚCٚC	ٚCٚCٚCٚCٚCٚCٚCٚCٚCٚCٚCٚC!ٚC#�4C%ٚC'ٚC)ٚC+ٚC-ٚC/ٚC1ٚC3ٚC5ٚC7ٚC9ٚC;ٚC=ٚC?ٚCAٚCCٚCEٚCGٚCIٚCKٚCMٚCOٚCQٚCSٚCUٚCWٚCYٚC[ٚC]ٚC_ٚCaٚCcٚCeٚCgٚCiٚCkٚCmٚCoٚCqٚCsٚCuٚCwٚCyٚC{ٚC}ٚCٚC���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���D vfD �fDvfD�fD|�D�fDvfD�fDvfD�fDvfD�fDvfD�fDvfD�fDvfD�fD	vfD	�fD
vfD
�fDvfD�fDvfD�fDvfD�fDvfD�fDvfD�fDvfD�fDvfD�fDvfD�fDvfD�fDvfD�fDvfD�fDvfD�fDvfD�fDvfD�fDvfD�fDvfD�fDvfD�fDvfD�fDvfD�fDvfD�fDvfD��D |�D �fD!vfD!�fD"vfD"�fD#vfD#�fD$vfD$�fD%vfD%�fD&vfD&�fD'vfD'�fD(vfD(�fD)vfD)�fD*vfD*�fD+vfD+�fD,vfD,�fD-vfD-�fD.vfD.�fD/vfD/�fD0vfD0�fD1vfD1�fD2vfD2�fD3vfD3�fD4vfD4�fD5vfD5�fD6vfD6�fD7vfD7�fD8vfD8�fD9vfD9�fD:vfD:�fD;vfD;�fD<vfD<�fD=vfD=�fD>vfD>�fD?vfD?�fD@vfD@�fDAvfDA�fDBvfDB�fDCvfDC�fDDvfDD�fDEvfDE�fDFvfDF�fDGvfDG�fDHvfDH�fDIvfDI�fDJvfDJ�fDKvfDK�fDLvfDL�fDMvfDM�fDNvfDN�fDOvfDO�fDPvfDP�fDQvfDQ�fDRvfDR�fDSvfDS�fDTvfDT�fDUvfDU�fDVvfDV�fDWvfDW�fDXvfDX�fDYvfDY�fDZvfDZ�fD[vfD[�fD\vfD\�fD]vfD]�fD^vfD^�fD_vfD_�fD`vfD`�fDavfDa�fDbvfDb�fDcvfDc�fDdvfDd�fDevfDe�fDfvfDf�fDgvfDg�fDhvfDh�fDivfDi�fDjvfDj�fDkvfDk�fDlvfDl�fDmvfDm�fDnvfDn�fDovfDo�fDpvfDp�fDqvfDq�fDrvfDr�fDsvfDs�fDtvfDt�fDuvfDu�fDvvfDv�fDwvfDw�fDxvfDx�fDyvfDy�fDzvfDz�fD{vfD{�fD|vfD|�fD}vfD}�fD~vfD~�fDvfD�fD�;3D�{3D��3D��3D�;3D�{3D��3D��3D�;3D�{3D��3D��3D�;3D�{3D��3D��3D�;3D�{3D��3D��3D�;3D�{3D��3D��3D�;3D�{3D��3D��3D�;3D�{3D��3D��3D�;3D�{3D��3D��3D�;3D�{3D��3D��3D�;3D�{3D��3D��3D�;3D�{3D��3D��3D�;3D�{3D��3D��3D�;3D�{3D��3D��3D�;3D�{3D��3D��3D�;3D�{3D��3D��3D�;3D�{3D��3D��3D�;3D�{3D��3D��3D�;3D�{3D��3D��3D�;3D�{3D��3D��3D�;3D�{3D��3D��3D�;3D�{3D��3D��3D�;3D�{3D��3D��3D�;3D�{3D��3D��3D�8 D�{3D��3D��3D�;3D�{3D��3D��3D�;3D�{3D��3D��3D�;3D�{3D��3D��3D�;3D�{3D��3D��3D�;3D�{3D��3D��3D�;3D�{3D��3D��3D�;3D�{3D��3D��3D�;3D�{3D��3D��3D�;3D�{3D��3D��3D�;3D�{3D��3D��3D�;3D�{3D��3D��3D�;3D�{3D��3D��fD�;3D�{3D��3D��3D�;3D�{3D��3D��3D�;3D�{3D��3D��3D�;3D�{3D��3D��3D�;3D�{3D��3D��3D�;3D�{3D��3D��3D�;3D�{3D��3D��3D�;3D�{3D��3D��3D�;3D�{3D��3D��3D�;3D�{3D��3D��3D�;3D�{3D��3D��3D�;3D�{3D��3D��3D�;3D�{3D��3D��3D�;3D�{3D��3D��3D�;3D�{3D��3D��3D�;3D�{3D��3D��3D�;3D�{3D��3D��3D�;3D�{3D��3D��3D�;3D�{3D��3D��3D�;3D�{3D��3D��3D�;3D�{3D��3D��3D�;3D�{3D��3D��3D�;3D�{3D��3D��3D�;3D�{3D��3D��3D�;3D�{3D��3D��3D�;3D�{3D��3D��3D�;3D�{3D��3D��3D�;3D�{3D��3D��3D�;3D�{3D��3D��fD�;3D�{3D»3D��3D�;3D�{3Dû3D��3D�;3D�{3DĻ3D��3D�;3D�{3DŻ3D��3D�;3D�{3Dƻ3D��3D�;3D�{3Dǻ3D��3D�;3D�{3DȻ3D��3D�;3D�{3Dɻ3D��3D�;3D�{3Dʻ3D��3D�;3D�{3D˻3D��3D�;3D�{3D̻3D��3D�;3D�{3Dͻ3D��3D�;3D�{3Dλ3D��3D�;3D�{3Dϻ3D��3D�;3D�{3Dл3D��3D�;3D�{3Dѻ3D��3D�;3D�{3Dһ3D��3D�;3D�{3Dӻ3D��3D�;3D�{3DԻ3D��3D�;3D�{3Dջ3D��3D�;3D�{3Dֻ3D��3D�;3D�{3D׻3D��3D�;3D�{3Dػ3D��3D�;3D�{3Dٻ3D��3D�;3D�{3Dڻ3D��3D�;3D�{3Dۻ3D��3D�;3D�{3Dܻ3D��3D�;3D�{3Dݻ3D��3D�;3D�{3D޻3D��3D�;3D�{3D߻3D��3D�;3D�{3D�3D��3D�;3D�{3D�3D��3D�;3D�{3D�3D��3D�;3D�{3D�3D��3D�;3D�{3D�3D��3D�;3D�{3D�3D��3D�;3D�{3D�3D��3D�;3D�{3D�3D��3D�;3D�{3D�3D��3D�;3D�{3D�3D��3D�;3D�{3D�3D��3D�;3D�{3D�3D��3D�;3D�{3D�3D��3D�;3D�{3D��3D��3D�;3D�{3D�3D��3D�;3D�{3D�3D��3D�;3D�{3D�3D��3D�;3D�{3D�3D��3D�;3D�{3D�3D��3D�;3D�{3D�3D��3D�;3D�{3D��3D��3D�;3D�{3D��3D��3D�;3D�~fD��f11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�A��RA��^A��jA��9A���A���A��+A�t�A�;dA���A���A�A��A��A�n�A�\)A�E�A�7LA�-A���A���A��A�G�A�{A��RA�oA�G�A�7LA��^A�K�A���A�S�A��;A��;A��;A�33A��yA��A���A�1'A��9A�A�`BA��FA�\)A�?}A���A�VA��A���A�1A��A�VA�`BA�\)A���A�G�A�bNA�z�A�`BA�JA�ffA�M�A��^A�;dA��jA�bNA���A�(�A��-A�bNA�33A���A���A�K�A�ZA�jA��7A�O�A���A�A�ƨA�$�A��-A��A�\)A� �A��A�9XA~��Ay�
Av��AuoAt�Ar��Ao�hAlr�Ak+Aj(�Ah��Ag�#Ag`BAfM�Ae�PAd(�Ac�Ab �A`�`A^�A^=qA]%AZ�AZbNAY�AX��AX$�AVv�AT�+AS��AShsASK�ASoAR�HARn�AR1AQAP��API�AO+ANffAM��AM�^AMC�AL�RAKx�AJ�AH�AG�
AF��AFbNAE��AE`BAD�uAC�
AC�wAB��AB �AA?}A?�hA>��A=A=
=A<��A<A;p�A;dZA:�+A9�A9�A8�jA8n�A7/A6Q�A3�A2~�A2��A3�A2�uA1x�A0n�A/�A/�A/+A-�mA-VA,�DA,{A+�PA+\)A*�A)��A)
=A(��A'�FA&�9A&(�A%/A$VA#�A#%A!�A ��A�^A��A1A|�AE�AĜAl�A��AM�A%A�A~�A��A�`A�AhsA��Ax�A��A�\AA�A��A/A�+A�FA7LA
-A1'A�wA�7AdZAM�AoA�wA+A�A��AQ�A�wA+A �+A �@���@���@�hs@�bN@�ƨ@��F@�t�@��@�5?@��9@�S�@���@�@�-@���@�j@��m@�t�@��@�+@�~�@�7@�z�@�t�@�G�@�I�@�P@���@�j@�ff@�7L@ߍP@�=q@݁@�G�@�V@���@�r�@ۍP@�@��@�1'@��@��@�ƨ@ѩ�@Ь@��
@�dZ@�v�@�p�@̬@�|�@�-@ǅ@�n�@��@ēu@��m@��
@��@�5?@�@�z�@���@��@�`B@��@�ȴ@���@���@���@��`@�9X@�\)@�ȴ@���@�(�@�ȴ@�@�x�@�j@���@���@�|�@�S�@�ff@�O�@��`@��@� �@���@�;d@���@�5?@��#@�X@���@��H@��+@�V@�@���@�p�@�Ĝ@��;@�|�@�;d@��H@��+@�M�@��@��T@��h@�&�@�r�@��;@�o@�v�@���@���@��@���@�j@�A�@�ƨ@�l�@�S�@�"�@���@�V@�=q@�@���@�G�@�&�@���@���@��@�1'@�1'@�1'@���@���@�S�@��@��R@��@��#@���@�`B@�/@��@�%@�j@�1@���@�C�@�o@�
=@���@���@�x�@��@��j@��@�Q�@�b@���@�
=@��!@�V@��@���@���@��@�/@�%@��@���@��9@��D@�(�@��;@��@�S�@�+@���@��@���@��R@���@�~�@�=q@���@�hs@���@��@�j@�(�@��
@��@�C�@��y@��!@��!@��!@�n�@�@��7@�/@���@��j@���@��D@��@�z�@�Z@�9X@�(�@�@�w@l�@;d@�@~ȴ@~��@~E�@}�@|��@{��@{��@{t�@{dZ@{C�@{o@z�\@z�@y�^@y&�@xĜ@x�u@xQ�@x  @w�@w\)@w�@vV@u@t�/@sƨ@s"�@r�@r��@rn�@r�@q��@qhs@q%@pĜ@p��@p�@pr�@pA�@o�@o��@n��@n5?@n@m��@m@m��@mO�@mV@lI�@k��@kdZ@j�@j��@j^5@i�@i��@iG�@h�9@hQ�@hb@gl�@g+@g
=@g
=@f�@f��@f��@fv�@f$�@e��@e�-@e�h@eO�@d�@dz�@d(�@co@b��@b��@b�!@b��@b-@a�@a��@`�`@`A�@` �@_�w@^�+@]�-@]O�@\��@\I�@\9X@\�@\1@[ƨ@["�@Z�H@Z^5@ZJ@Y��@Y��@Yx�@XĜ@X��@X�u@X�u@X�u@X�@X1'@W+@V��@Vȴ@V�+@VE�@V{@U@U�-@Up�@T��@T�j@Tz�@TI�@T1@S��@SS�@S33@S"�@So@R�H@R��@R��@Rn�@R^5@R=q@R�@Q��@Q��@Qx�@Q�@P��@P��@Pr�@PbN@PA�@P1'@O��@O
=@Nv�@N{@M@M��@M?}@L�/@Lz�@K��@K��@K��@K�
@K�
@K�
@K�F@K��@K��@K��@K�@Kt�@KdZ@KdZ@KS�@K33@J��@J�\@J^5@J=q@J=q@J-@J-@J�@J�@JJ@JJ@JJ@I��@I�@I�@I�^@IX@H�`@HĜ@Hr�@G�;@G�@G�P@G\)@G;d@F�@F��@F��@F�+@Fff@FE�@E��@E�h@D�/@D�@D��@D�D@DI�@C��@C�F@Ct�@CS�@C33@Bn�@A�@@Q�@?�;@?��@?|�@?\)@?+@?
=@>�y@>�@>�R@>��@>v�@>5?@=�@=�T@=�-@=�h@=p�@=?}@;ƨ@:M�@9��@9��@9�7@9X@9&�@8�`@8�9@8bN@8b@7�@7��@7�w@7��@7\)@7K�@6��@6$�@5p�@4�@4z�@4(�@3��@3�F@3��@3��@3�@3t�@3t�@3dZ@3dZ@3dZ@3o@2~�@2=q@1%@0�9@0�@0A�@0A�@0 �@/��@/K�@/+@.�@.�R@.��@.�+@.ff@.v�@.v�@.v�@.v�@.ff@.V@.E�@.E�@.5?@-�@-�T@-p�@,j@+t�@+"�@+o@+o@*�@*�\@*n�@*M�@*J@)�#@)�#@)�#@)�#@)�#@)��@)��@)��@)��@)G�@)%@(�@(r�@(r�@(b@'�;@'�P@';d@'+@'�@'
=@&��@&�@&�+@&E�@&@%��@%�h@%p�@%p�@$��@$I�@#ƨ@#��@#dZ@#S�@#33@"�@"�H@"�@"�H@"n�@"~�@"^5@"=q@"�@"J@"J@"J@"J@"J@"J@!��@!�^@!�^@!��@!�7@!G�@!&�@!�@ ��@ �@�@|�@l�@K�@K�@K�@K�@;d@
=@�R@�R@��@��@��@�+@v�@V@V@V@E�@@��@@�-@�-@�-@��@�h@�@�@�@�/@I�@1@ƨ@��@dZ@@�H@��@^5@�@�^@�@Ĝ@bN@�w@l�@K�@
=@��@V@$�@{@{@�T@@�-@��@�h@�@`B@O�@?}@�@�@��@�D@Z@I�@(�@1@�@�@1@�m@��@��@dZ@C�@33@o@o@�@��@~�@n�@^5@^5@M�@-@�#@��@��@7L@��@r�@Q�@b@�@\)@+@�@�y@5?@�@@�h@�@�@�@p�@`B@/@�/@Z@�@�
@�@C�@33@"�@
��@
�!@
~�@
^5@
M�@
=q@
J@	�#@	�^@	��@	x�@	hs@	X@	G�@	7L@	�@��@�`@�9@bN@A�@1'@�@��@�@|�@\)@;d@�y@��@v�@E�@$�@�T11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  A��RA��^A��jA��9A���A���A��+A�t�A�;dA���A���A�A��A��A�n�A�\)A�E�A�7LA�-A���A���A��A�G�A�{A��RA�oA�G�A�7LA��^A�K�A���A�S�A��;A��;A��;A�33A��yA��A���A�1'A��9A�A�`BA��FA�\)A�?}A���A�VA��A���A�1A��A�VA�`BA�\)A���A�G�A�bNA�z�A�`BA�JA�ffA�M�A��^A�;dA��jA�bNA���A�(�A��-A�bNA�33A���A���A�K�A�ZA�jA��7A�O�A���A�A�ƨA�$�A��-A��A�\)A� �A��A�9XA~��Ay�
Av��AuoAt�Ar��Ao�hAlr�Ak+Aj(�Ah��Ag�#Ag`BAfM�Ae�PAd(�Ac�Ab �A`�`A^�A^=qA]%AZ�AZbNAY�AX��AX$�AVv�AT�+AS��AShsASK�ASoAR�HARn�AR1AQAP��API�AO+ANffAM��AM�^AMC�AL�RAKx�AJ�AH�AG�
AF��AFbNAE��AE`BAD�uAC�
AC�wAB��AB �AA?}A?�hA>��A=A=
=A<��A<A;p�A;dZA:�+A9�A9�A8�jA8n�A7/A6Q�A3�A2~�A2��A3�A2�uA1x�A0n�A/�A/�A/+A-�mA-VA,�DA,{A+�PA+\)A*�A)��A)
=A(��A'�FA&�9A&(�A%/A$VA#�A#%A!�A ��A�^A��A1A|�AE�AĜAl�A��AM�A%A�A~�A��A�`A�AhsA��Ax�A��A�\AA�A��A/A�+A�FA7LA
-A1'A�wA�7AdZAM�AoA�wA+A�A��AQ�A�wA+A �+A �@���@���@�hs@�bN@�ƨ@��F@�t�@��@�5?@��9@�S�@���@�@�-@���@�j@��m@�t�@��@�+@�~�@�7@�z�@�t�@�G�@�I�@�P@���@�j@�ff@�7L@ߍP@�=q@݁@�G�@�V@���@�r�@ۍP@�@��@�1'@��@��@�ƨ@ѩ�@Ь@��
@�dZ@�v�@�p�@̬@�|�@�-@ǅ@�n�@��@ēu@��m@��
@��@�5?@�@�z�@���@��@�`B@��@�ȴ@���@���@���@��`@�9X@�\)@�ȴ@���@�(�@�ȴ@�@�x�@�j@���@���@�|�@�S�@�ff@�O�@��`@��@� �@���@�;d@���@�5?@��#@�X@���@��H@��+@�V@�@���@�p�@�Ĝ@��;@�|�@�;d@��H@��+@�M�@��@��T@��h@�&�@�r�@��;@�o@�v�@���@���@��@���@�j@�A�@�ƨ@�l�@�S�@�"�@���@�V@�=q@�@���@�G�@�&�@���@���@��@�1'@�1'@�1'@���@���@�S�@��@��R@��@��#@���@�`B@�/@��@�%@�j@�1@���@�C�@�o@�
=@���@���@�x�@��@��j@��@�Q�@�b@���@�
=@��!@�V@��@���@���@��@�/@�%@��@���@��9@��D@�(�@��;@��@�S�@�+@���@��@���@��R@���@�~�@�=q@���@�hs@���@��@�j@�(�@��
@��@�C�@��y@��!@��!@��!@�n�@�@��7@�/@���@��j@���@��D@��@�z�@�Z@�9X@�(�@�@�w@l�@;d@�@~ȴ@~��@~E�@}�@|��@{��@{��@{t�@{dZ@{C�@{o@z�\@z�@y�^@y&�@xĜ@x�u@xQ�@x  @w�@w\)@w�@vV@u@t�/@sƨ@s"�@r�@r��@rn�@r�@q��@qhs@q%@pĜ@p��@p�@pr�@pA�@o�@o��@n��@n5?@n@m��@m@m��@mO�@mV@lI�@k��@kdZ@j�@j��@j^5@i�@i��@iG�@h�9@hQ�@hb@gl�@g+@g
=@g
=@f�@f��@f��@fv�@f$�@e��@e�-@e�h@eO�@d�@dz�@d(�@co@b��@b��@b�!@b��@b-@a�@a��@`�`@`A�@` �@_�w@^�+@]�-@]O�@\��@\I�@\9X@\�@\1@[ƨ@["�@Z�H@Z^5@ZJ@Y��@Y��@Yx�@XĜ@X��@X�u@X�u@X�u@X�@X1'@W+@V��@Vȴ@V�+@VE�@V{@U@U�-@Up�@T��@T�j@Tz�@TI�@T1@S��@SS�@S33@S"�@So@R�H@R��@R��@Rn�@R^5@R=q@R�@Q��@Q��@Qx�@Q�@P��@P��@Pr�@PbN@PA�@P1'@O��@O
=@Nv�@N{@M@M��@M?}@L�/@Lz�@K��@K��@K��@K�
@K�
@K�
@K�F@K��@K��@K��@K�@Kt�@KdZ@KdZ@KS�@K33@J��@J�\@J^5@J=q@J=q@J-@J-@J�@J�@JJ@JJ@JJ@I��@I�@I�@I�^@IX@H�`@HĜ@Hr�@G�;@G�@G�P@G\)@G;d@F�@F��@F��@F�+@Fff@FE�@E��@E�h@D�/@D�@D��@D�D@DI�@C��@C�F@Ct�@CS�@C33@Bn�@A�@@Q�@?�;@?��@?|�@?\)@?+@?
=@>�y@>�@>�R@>��@>v�@>5?@=�@=�T@=�-@=�h@=p�@=?}@;ƨ@:M�@9��@9��@9�7@9X@9&�@8�`@8�9@8bN@8b@7�@7��@7�w@7��@7\)@7K�@6��@6$�@5p�@4�@4z�@4(�@3��@3�F@3��@3��@3�@3t�@3t�@3dZ@3dZ@3dZ@3o@2~�@2=q@1%@0�9@0�@0A�@0A�@0 �@/��@/K�@/+@.�@.�R@.��@.�+@.ff@.v�@.v�@.v�@.v�@.ff@.V@.E�@.E�@.5?@-�@-�T@-p�@,j@+t�@+"�@+o@+o@*�@*�\@*n�@*M�@*J@)�#@)�#@)�#@)�#@)�#@)��@)��@)��@)��@)G�@)%@(�@(r�@(r�@(b@'�;@'�P@';d@'+@'�@'
=@&��@&�@&�+@&E�@&@%��@%�h@%p�@%p�@$��@$I�@#ƨ@#��@#dZ@#S�@#33@"�@"�H@"�@"�H@"n�@"~�@"^5@"=q@"�@"J@"J@"J@"J@"J@"J@!��@!�^@!�^@!��@!�7@!G�@!&�@!�@ ��@ �@�@|�@l�@K�@K�@K�@K�@;d@
=@�R@�R@��@��@��@�+@v�@V@V@V@E�@@��@@�-@�-@�-@��@�h@�@�@�@�/@I�@1@ƨ@��@dZ@@�H@��@^5@�@�^@�@Ĝ@bN@�w@l�@K�@
=@��@V@$�@{@{@�T@@�-@��@�h@�@`B@O�@?}@�@�@��@�D@Z@I�@(�@1@�@�@1@�m@��@��@dZ@C�@33@o@o@�@��@~�@n�@^5@^5@M�@-@�#@��@��@7L@��@r�@Q�@b@�@\)@+@�@�y@5?@�@@�h@�@�@�@p�@`B@/@�/@Z@�@�
@�@C�@33@"�@
��@
�!@
~�@
^5@
M�@
=q@
J@	�#@	�^@	��@	x�@	hs@	X@	G�@	7L@	�@��@�`@�9@bN@A�@1'@�@��@�@|�@\)@;d@�y@��@v�@E�@$�@�T11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�B�'B�'B�'B�!B�B�B�B�B�B�B�B�B�B��B��B��B��B��B��B��B�\B�B|�By�Bq�BbNBZBQ�BD�B;dB6FB33B8RB5?B<jBG�BK�BN�BN�BM�BC�BI�BI�BF�B?}B,BJB�B�TB��B��B��B�oB�Br�BcTB\)BL�B:^B%�B#�B�BDBB��B�B�B�HB�B��BǮB��B�LB��Bo�BP�B>wB0!B�B	7B��B��B�B�fB�TB�HB�/B�
B��B�}B��B�PB� Bv�Bo�BW
BF�B=qB7LB2-B-B,B'�B%�B�B�B�B�BDB+BB��B��B�B�B�B�TB�/B�;B�5B�5B�/B�/B�/B�)B�/B�B�B��B��B��B��B��B��BȴB��B�}B�dB�9B�'B�B�B��B��B��B��B��B��B�VB�=B�B�B�B�B� B�B}�B{�Bx�Bw�Bu�Bq�BiyB[#BL�BR�Be`BhsBdZB`BB]/B[#BXBR�BN�BM�BK�BK�BK�BF�BC�B=qB9XB6FB2-B0!B-B)�B(�B&�B$�B!�B�B�B�B�B�B�BbBVBJB+BBBB
��B
��B
��B
��B
��B
��B
��B
��B
��B
�B
�B
�B
�B
�B
�B
�sB
�mB
�fB
�`B
�ZB
�TB
�HB
�HB
�BB
�BB
�;B
�5B
�/B
�)B
�)B
�#B
�#B
�#B
�B
�B
�B
�B
�B
�
B
�
B
�
B
�B
�B
�
B
�
B
�
B
�
B
�
B
�
B
�B
�
B
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
�B
�B
�B
��B
�B
�B
�B
�#B
�)B
�#B
�/B
�/B
�/B
�;B
�BB
�BB
�TB
�fB
�sB
�B
�B
�B
��B
��B
��B
��B
��BB1B1B	7BDBPBbBoBoB�B�B�B�B�B �B!�B#�B%�B&�B'�B0!B33B5?B6FB8RB9XB:^B=qBB�BC�BD�BF�BH�BI�BJ�BK�BL�BN�BR�BVBZB_;BbNBcTBhsBk�Bl�Bm�Bp�Bq�Br�Bs�Bu�Bw�By�B{�B}�B� B� B�B�B�B�%B�%B�%B�+B�=B�JB�PB�bB��B��B��B��B��B��B��B��B��B��B�B�B�B�B�?B�XB�dB�wB��BBĜBǮB��B��B��B��B��B��B�B�B�#B�5B�;B�BB�HB�`B�mB�sB�B�B�B�B�B�B�B�B��B��B��B  BBB%B	7BDBPBhBoBoBoB{B�B�B�B �B"�B#�B$�B$�B$�B&�B'�B'�B(�B)�B,B,B-B.B.B0!B1'B7LB9XB:^B;dB<jB<jB=qB?}BA�BC�BF�BG�BH�BI�BJ�BL�BM�BN�BS�BVB[#B`BBcTBdZBe`BgmBhsBiyBk�Bm�Bn�Bo�Bp�Bp�Bq�Br�Bt�Bx�Bz�Bz�B{�B|�B|�B~�B� B�B�%B�+B�7B�=B�DB�PB�VB�bB�oB�{B�{B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B�B�B�B�B�B�B�'B�3B�3B�?B�XB�jB�qB�wB��B��B��B��BBÖBÖBŢBƨBǮBȴBȴB��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B�B�
B�B�B�B�B�#B�)B�)B�)B�)B�/B�/B�/B�5B�5B�;B�;B�BB�BB�NB�TB�TB�ZB�ZB�`B�`B�fB�sB�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B  B  BBBBBBBBBB%B%B%B+B
=BDBJBPBPBPBVBVBVB\B\B\B\BbBbBhBhBhBhBhB{B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B �B!�B!�B"�B"�B"�B#�B#�B#�B#�B#�B#�B#�B$�B$�B%�B'�B(�B(�B(�B(�B(�B)�B+B+B,B,B,B,B-B-B-B-B-B-B-B.B-B.B.B.B/B1'B2-B33B33B33B33B49B49B49B5?B5?B5?B5?B5?B6FB6FB6FB6FB6FB7LB7LB8RB8RB8RB9XB9XB9XB:^B:^B:^B:^B:^B:^B;dB;dB<jB=qB=qB=qB=qB>wB?}B?}B@�B@�B@�BA�BA�BA�BA�BA�BB�BB�BB�BB�BB�BC�BC�BC�BC�BC�BC�BC�BC�BD�BC�BD�BD�BD�BE�BE�BE�BG�BG�BG�BG�BG�BH�BH�BH�BH�BI�BI�BI�BI�BI�BI�BI�BI�BI�BI�BI�BJ�BJ�BJ�BJ�BJ�BJ�BK�BK�BK�BK�BL�BL�BM�BM�BM�BN�BN�BO�BO�BO�BO�BP�BP�BQ�BR�BR�BS�BT�BT�BT�BVBVBW
BW
BW
BW
BXBXBXBXBXBXBXBYBYBYBZBZBZBZBZB[#B[#B[#B[#B[#B[#B\)B\)B\)B\)B]/B]/B]/B]/B^5B^5B^5B^5B^5B^5B_;B_;B_;B`BB`BBaHBaHBaHBbNBbNBcTBcTBcTBdZBe`Be`Be`Be`Be`Be`Be`BffBffBffBgmBgmBhsBiyBiyBiyBiyBjBjBjBjBk�Bk�Bk�Bk�Bl�Bl�Bl�Bl�Bl�Bm�Bm�Bm�Bm�Bm�Bm�Bn�Bn�Bn�Bo�Bo�Bo�Bo�Bp�Bp�Bp�Bq�Bq�Br�Br�Br�44444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444  B�'B�'B�'B�!B�B�B�B�B�B�B�B�B�B��B��B��B��B��B��B��B�\B�B|�By�Bq�BbNBZBQ�BD�B;dB6FB33B8RB5?B<jBG�BK�BN�BN�BM�BC�BI�BI�BF�B?}B,BJB�B�TB��B��B��B�oB�Br�BcTB\)BL�B:^B%�B#�B�BDBB��B�B�B�HB�B��BǮB��B�LB��Bo�BP�B>wB0!B�B	7B��B��B�B�fB�TB�HB�/B�
B��B�}B��B�PB� Bv�Bo�BW
BF�B=qB7LB2-B-B,B'�B%�B�B�B�B�BDB+BB��B��B�B�B�B�TB�/B�;B�5B�5B�/B�/B�/B�)B�/B�B�B��B��B��B��B��B��BȴB��B�}B�dB�9B�'B�B�B��B��B��B��B��B��B�VB�=B�B�B�B�B� B�B}�B{�Bx�Bw�Bu�Bq�BiyB[#BL�BR�Be`BhsBdZB`BB]/B[#BXBR�BN�BM�BK�BK�BK�BF�BC�B=qB9XB6FB2-B0!B-B)�B(�B&�B$�B!�B�B�B�B�B�B�BbBVBJB+BBBB
��B
��B
��B
��B
��B
��B
��B
��B
��B
�B
�B
�B
�B
�B
�B
�sB
�mB
�fB
�`B
�ZB
�TB
�HB
�HB
�BB
�BB
�;B
�5B
�/B
�)B
�)B
�#B
�#B
�#B
�B
�B
�B
�B
�B
�
B
�
B
�
B
�B
�B
�
B
�
B
�
B
�
B
�
B
�
B
�B
�
B
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
�B
�B
�B
��B
�B
�B
�B
�#B
�)B
�#B
�/B
�/B
�/B
�;B
�BB
�BB
�TB
�fB
�sB
�B
�B
�B
��B
��B
��B
��B
��BB1B1B	7BDBPBbBoBoB�B�B�B�B�B �B!�B#�B%�B&�B'�B0!B33B5?B6FB8RB9XB:^B=qBB�BC�BD�BF�BH�BI�BJ�BK�BL�BN�BR�BVBZB_;BbNBcTBhsBk�Bl�Bm�Bp�Bq�Br�Bs�Bu�Bw�By�B{�B}�B� B� B�B�B�B�%B�%B�%B�+B�=B�JB�PB�bB��B��B��B��B��B��B��B��B��B��B�B�B�B�B�?B�XB�dB�wB��BBĜBǮB��B��B��B��B��B��B�B�B�#B�5B�;B�BB�HB�`B�mB�sB�B�B�B�B�B�B�B�B��B��B��B  BBB%B	7BDBPBhBoBoBoB{B�B�B�B �B"�B#�B$�B$�B$�B&�B'�B'�B(�B)�B,B,B-B.B.B0!B1'B7LB9XB:^B;dB<jB<jB=qB?}BA�BC�BF�BG�BH�BI�BJ�BL�BM�BN�BS�BVB[#B`BBcTBdZBe`BgmBhsBiyBk�Bm�Bn�Bo�Bp�Bp�Bq�Br�Bt�Bx�Bz�Bz�B{�B|�B|�B~�B� B�B�%B�+B�7B�=B�DB�PB�VB�bB�oB�{B�{B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B�B�B�B�B�B�B�'B�3B�3B�?B�XB�jB�qB�wB��B��B��B��BBÖBÖBŢBƨBǮBȴBȴB��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B�B�
B�B�B�B�B�#B�)B�)B�)B�)B�/B�/B�/B�5B�5B�;B�;B�BB�BB�NB�TB�TB�ZB�ZB�`B�`B�fB�sB�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B  B  BBBBBBBBBB%B%B%B+B
=BDBJBPBPBPBVBVBVB\B\B\B\BbBbBhBhBhBhBhB{B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B �B!�B!�B"�B"�B"�B#�B#�B#�B#�B#�B#�B#�B$�B$�B%�B'�B(�B(�B(�B(�B(�B)�B+B+B,B,B,B,B-B-B-B-B-B-B-B.B-B.B.B.B/B1'B2-B33B33B33B33B49B49B49B5?B5?B5?B5?B5?B6FB6FB6FB6FB6FB7LB7LB8RB8RB8RB9XB9XB9XB:^B:^B:^B:^B:^B:^B;dB;dB<jB=qB=qB=qB=qB>wB?}B?}B@�B@�B@�BA�BA�BA�BA�BA�BB�BB�BB�BB�BB�BC�BC�BC�BC�BC�BC�BC�BC�BD�BC�BD�BD�BD�BE�BE�BE�BG�BG�BG�BG�BG�BH�BH�BH�BH�BI�BI�BI�BI�BI�BI�BI�BI�BI�BI�BI�BJ�BJ�BJ�BJ�BJ�BJ�BK�BK�BK�BK�BL�BL�BM�BM�BM�BN�BN�BO�BO�BO�BO�BP�BP�BQ�BR�BR�BS�BT�BT�BT�BVBVBW
BW
BW
BW
BXBXBXBXBXBXBXBYBYBYBZBZBZBZBZB[#B[#B[#B[#B[#B[#B\)B\)B\)B\)B]/B]/B]/B]/B^5B^5B^5B^5B^5B^5B_;B_;B_;B`BB`BBaHBaHBaHBbNBbNBcTBcTBcTBdZBe`Be`Be`Be`Be`Be`Be`BffBffBffBgmBgmBhsBiyBiyBiyBiyBjBjBjBjBk�Bk�Bk�Bk�Bl�Bl�Bl�Bl�Bl�Bm�Bm�Bm�Bm�Bm�Bm�Bn�Bn�Bn�Bo�Bo�Bo�Bo�Bp�Bp�Bp�Bq�Bq�Br�Br�Br�44444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444  G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�PRES            TEMP            PSAL            PRES_ADJUSTED = PRES - surface_pressure                                                                                                                                                                                                                         none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            surface_pressure=0.15 dbar                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                      Pressure adjusted at real time based on most recent valid surface pressure                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                      20220514183558                              AO  ARCAADJP                                                                    20220514183558    IP                G�O�G�O�G�O�                AO  ARGQQCPL                                                                    20220514183558  QCP$                G�O�G�O�G�O�5F03E           AO  ARGQQCPL                                                                    20220514183558  QCF$                G�O�G�O�G�O�8000            