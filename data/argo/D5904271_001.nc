CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  f   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       2014-07-22T01:11:08Z creation      
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
resolution        =���     �  J0   PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 h  W�   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         decibar    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  [0   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  h�   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 h  v`   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  y�   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 h  �`   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         degree_Celsius     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  ��   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  �`   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 h  ��   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  �`   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 h  ��   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  �`   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  0  ��   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                    �(   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                    �(   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                    �(   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    
_FillValue               conventions       YYYYMMDDHHMISS        ,  �(   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                    є   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                    ј   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                    ќ   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                    Ѡ   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  @  Ѥ   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                    ��   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                    ��   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    ��   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar         �   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar         �   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�        �   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    �   FLOAT_SERIAL_NO                   	long_name         Serial number of the float     
_FillValue                     �T   FIRMWARE_VERSION                  	long_name         Instrument firmware version    
_FillValue                     �t         �tArgo profile    3.1 1.2 19500101000000  20140722011108  20181023142215  5904271 US ARGO PROJECT                                                 GREGORY C. JOHNSON                                              PRES            TEMP            PSAL               A   AO  4744_0188_001                   2C  D   NAVIS_A                         863 @֓�DD@ 1   @֓��@ @+0��
=q�c�C��%1   GPS     Primary sampling: averaged [1Hz sampling by SBE-41CP averaged in 2-dbar bins]                                                                                                                                                                                      A   A   A   @�ff@�  A   A   A@  A`  A�  A�  A�  A�  A�  A�  A�  A�  B   B  B  B  B   B(  B0  B8  B@  BH  BP  BX  B`ffBh  Bp  Bx  B�  B�  B�33B�  B���B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�33B�  B�  B�  B�  B�  B�  B�  B�  B�33B�  B�  B�  B�  B�  B�  B�  C   C  C  C  C  C
  C  C  C  C  C  C  C  C  C  C  C �C"�C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dt� Du  Du� Dv  Dv� Dw  Dw� Dx  Dx� Dy  Dy� Dz  Dz� D{  D{� D|  D|� D}  D}� D~  D~� D  D� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D D�� D�  D�@ DÀ D�� D�  D�@ DĀ D�� D�  D�@ Dŀ D�� D�  D�@ Dƀ D�� D�  D�@ Dǀ D�� D�  D�@ DȀ D�� D�  D�@ Dɀ D�� D�  D�@ Dʀ D�� D�  D�@ Dˀ D�� D�  D�@ D̀ D�� D�  D�@ D̀ D�� D�  D�@ D΀ D�� D�  D�@ Dπ D�� D�  D�@ DЀ D�� D�  D�@ Dр D�� D�  D�@ DҀ D�� D�  D�@ DӀ D�� D�  D�@ DԀ D�� D�  D�@ DՀ D�� D�  D�@ Dր D�� D�  D�@ D׀ D�� D�  D�@ D؀ D�� D�  D�@ Dك3Dٶf111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  @���@�33@�33A��A=��A]��A}��A���A���A���A���A���A���A���A���BffBffBffBffB'ffB/ffB7ffB?ffBGffBOffBWffB_��BgffBoffBwffBffB��3B��fB��3B�� B��3B��3B��3B��3B��3B��3B��3B��3B��3B��3B��fB��3Bó3Bǳ3B˳3Bϳ3Bӳ3B׳3B۳3B��fB�3B�3B�3B�3B�3B��3B��3B��3CٚCٚCٚCٚC	ٚCٚCٚCٚCٚCٚCٚCٚCٚCٚCٚC�4C!�4C#ٚC%ٚC'ٚC)ٚC+ٚC-ٚC/ٚC1ٚC3ٚC5ٚC7ٚC9ٚC;ٚC=ٚC?ٚCAٚCCٚCEٚCGٚCIٚCKٚCMٚCOٚCQٚCSٚCUٚCWٚCYٚC[ٚC]ٚC_ٚCaٚCcٚCeٚCgٚCiٚCkٚCmٚCoٚCqٚCsٚCuٚCwٚCyٚC{ٚC}ٚCٚC���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C�� C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���D vfD �fDvfD�fDvfD�fDvfD�fDvfD�fDvfD�fDvfD�fDvfD�fDvfD�fD	vfD	�fD
vfD
�fDvfD�fDvfD�fDvfD�fDvfD�fDvfD�fDvfD�fDvfD�fDvfD�fDvfD�fDvfD�fDvfD�fDvfD�fDvfD�fDvfD�fDvfD�fDvfD�fDvfD�fDvfD�fDvfD�fDvfD�fDvfD�fD vfD �fD!vfD!�fD"vfD"�fD#vfD#�fD$vfD$�fD%vfD%�fD&vfD&�fD'vfD'�fD(vfD(�fD)vfD)�fD*vfD*�fD+vfD+�fD,vfD,�fD-vfD-�fD.vfD.�fD/vfD/�fD0vfD0�fD1vfD1�fD2vfD2�fD3vfD3�fD4vfD4�fD5vfD5�fD6vfD6�fD7vfD7�fD8vfD8�fD9vfD9�fD:vfD:�fD;vfD;�fD<vfD<�fD=vfD=�fD>vfD>�fD?vfD?�fD@vfD@�fDAvfDA�fDBvfDB�fDCvfDC�fDDvfDD�fDEvfDE�fDFvfDF�fDGvfDG�fDHvfDH�fDIvfDI�fDJvfDJ�fDKvfDK�fDLvfDL�fDMvfDM�fDNvfDN�fDOvfDO�fDPvfDP�fDQvfDQ�fDRvfDR�fDSvfDS�fDTvfDT�fDUvfDU�fDVvfDV�fDWvfDW�fDXvfDX�fDYvfDY�fDZvfDZ�fD[vfD[�fD\vfD\�fD]vfD]�fD^vfD^�fD_vfD_�fD`vfD`�fDavfDa�fDbvfDb�fDcvfDc�fDdvfDd�fDevfDe�fDfvfDf�fDgvfDg�fDhvfDh�fDivfDi�fDjvfDj�fDkvfDk�fDlvfDl�fDmvfDm�fDnvfDn�fDovfDo�fDpvfDp�fDqvfDq�fDrvfDr�fDsvfDs�fDtvfDt�fDuvfDu�fDvvfDv�fDwvfDw�fDxvfDx�fDyvfDy�fDzvfDz�fD{vfD{�fD|vfD|�fD}vfD}�fD~vfD~�fDvfD�fD�;3D�{3D��3D��3D�;3D�{3D��3D��3D�;3D�{3D��3D��3D�;3D�{3D��3D��3D�;3D�{3D��3D��3D�;3D�{3D��3D��3D�;3D�{3D��3D��3D�;3D�{3D��3D��3D�;3D�{3D��3D��3D�;3D�{3D��3D��3D�;3D�{3D��3D��3D�;3D�{3D��3D��3D�;3D�{3D��3D��3D�;3D�{3D��3D��3D�;3D�{3D��3D��3D�;3D�{3D��3D��3D�;3D�{3D��3D��3D�;3D�{3D��3D��3D�;3D�{3D��3D��3D�;3D�{3D��3D��3D�;3D�{3D��3D��3D�;3D�{3D��3D��3D�;3D�{3D��3D��3D�;3D�{3D��3D��3D�;3D�{3D��3D��3D�;3D�{3D��3D��3D�;3D�{3D��3D��3D�;3D�{3D��3D��3D�;3D�{3D��3D��3D�;3D�{3D��3D��3D�;3D�{3D��3D��3D�;3D�{3D��3D��3D�;3D�{3D��3D��3D�;3D�{3D��3D��3D�;3D�{3D��3D��3D�;3D�{3D��3D��3D�;3D�{3D��3D��3D�;3D�{3D��3D��3D�;3D�{3D��3D��3D�;3D�{3D��3D��3D�;3D�{3D��3D��3D�;3D�{3D��3D��3D�;3D�{3D��3D��3D�;3D�{3D��3D��3D�;3D�{3D��3D��3D�;3D�{3D��3D��3D�;3D�{3D��3D��3D�;3D�{3D��3D��3D�;3D�{3D��3D��3D�;3D�{3D��3D��3D�;3D�{3D��3D��3D�;3D�{3D��3D��3D�;3D�{3D��3D��3D�;3D�{3D��3D��3D�;3D�{3D��3D��3D�;3D�{3D��3D��3D�;3D�{3D��3D��3D�;3D�{3D��3D��3D�;3D�{3D��3D��3D�;3D�{3D��3D��3D�;3D�{3D��3D��3D�;3D�{3D��3D��3D�;3D�{3D��3D��3D�;3D�{3D��3D��3D�;3D�{3D��3D��3D�;3D�{3D��3D��3D�;3D�{3D»3D��3D�;3D�{3Dû3D��3D�;3D�{3DĻ3D��3D�;3D�{3DŻ3D��3D�;3D�{3Dƻ3D��3D�;3D�{3Dǻ3D��3D�;3D�{3DȻ3D��3D�;3D�{3Dɻ3D��3D�;3D�{3Dʻ3D��3D�;3D�{3D˻3D��3D�;3D�{3D̻3D��3D�;3D�{3Dͻ3D��3D�;3D�{3Dλ3D��3D�;3D�{3Dϻ3D��3D�;3D�{3Dл3D��3D�;3D�{3Dѻ3D��3D�;3D�{3Dһ3D��3D�;3D�{3Dӻ3D��3D�;3D�{3DԻ3D��3D�;3D�{3Dջ3D��3D�;3D�{3Dֻ3D��3D�;3D�{3D׻3D��3D�;3D�{3Dػ3D��3D�;3D�~fDٱ�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  @��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��A��A���Aҟ�AҍPA�r�A��A�A���A��A��A��yA��`A��TA��HA��HA��/A��#A��A��A���A���A���A���A�ƨAѸRAэPA�`BA� �AЋDA��A�/A�ZA�A�(�A�$�A�$�A��A��TA���A�E�A�XA��A��mA�^5A���A�p�A�+A���A�ȴA�bNA��A�l�A��A���A���A�7LA�S�A�1'A�?}A��RA���A�bA�VA�^5A���A���A��A�z�A��DA�-A�|�A��A���A�
=A��+A��;A��yA���A�|�A��A��wA}��Ay�mAtffAr�uAl��Ai�wAgt�Ad^5AaAZ��AUdZARI�AN�uALz�AJE�AH��AE&�ABȴAA\)A?��A<��A:=qA7��A5�hA4�A3�A1dZA/�A.r�A-��A,�jA,n�A+dZA+?}A*��A*-A)�A'��A&�HA&�A&(�A#�A$Q�A%A&�A&��A'�A'/A'S�A'+A'A&ĜA%l�A#�A"�\A"E�A"(�A!��A!/A ��A�A��A;dAA�An�A��A��Al�A�TAVAbA�`A�\A1'A�A��A|�AS�A�jA�7A�AoA�AA�A��A|�A��A��AM�A�FA33A1'A��A��AC�A��A9XA�TA�wA�A?}A�A�/A��A5?A �A�#A��AK�A��A�/A��Az�AQ�A  AƨA?}A�AbA�AK�A
�A
r�A	�A	�wA	��A	p�A	?}A	%A�`AĜAM�A�AJA��AO�A�AȴA-A�TA��A�AC�A%A�HA��AA�A��A�^At�A
=AĜAv�AA�A��AXA%A ��A =q@��@�@�=q@��h@��@��D@�ƨ@��@��R@�n�@�p�@�j@�+@���@��-@�p�@�?}@���@�Ĝ@�A�@���@�"�@�^5@�X@�1'@�"�@���@�-@���@��@�Z@�@��y@�{@�V@�j@���@�dZ@���@�!@�+@��@��@�z�@���@�F@�
=@�+@�5?@���@�x�@�7L@�%@���@��u@�K�@��@ް!@ޏ\@�v�@�n�@�@ݙ�@�?}@��@�z�@��@ۍP@�dZ@�\)@��@�E�@�@���@�r�@��m@�K�@�^5@��`@�Z@�9X@�b@ӥ�@ҏ\@Ѳ-@��@Гu@�A�@�33@�M�@ͩ�@��`@�r�@��m@˕�@�l�@�o@���@�`B@��@�A�@ǅ@�\)@Ƨ�@��@��@���@ź^@�X@ě�@���@�"�@�=q@��#@�p�@��@��u@�bN@�9X@��@�S�@�33@��y@�-@��#@��-@�%@�Q�@��@��@���@�|�@�S�@�@���@��+@�@�&�@��9@�Z@��@���@��y@��R@�v�@��7@�7L@���@�z�@���@�;d@�
=@�@���@���@�V@��-@�x�@�V@�1'@��w@���@�;d@�E�@�@�x�@�%@��/@�Q�@���@��;@���@�l�@�l�@�\)@�33@��@�~�@�V@�=q@�-@��@�G�@��`@�1'@�S�@��@��\@�E�@��^@���@��7@�x�@�X@��@�%@���@��u@�A�@�1@���@�ƨ@�
=@�n�@�5?@��-@�/@��@�z�@�j@�1'@��m@��@�+@��H@��R@��\@�^5@��@�X@��@��@�Ĝ@��9@��@�j@�9X@��m@�ƨ@�t�@�;d@�"�@���@��!@�-@��@���@�&�@��@���@�j@�(�@���@�t�@�C�@�+@��y@��\@�V@�-@��@��-@�hs@�7L@���@��@�r�@�I�@�b@���@�l�@��@�ff@�{@��@���@�?}@��/@��9@�(�@���@��@��;@��F@���@���@�t�@�;d@��H@���@�~�@�n�@�n�@�ff@�-@��-@�x�@��@��u@�bN@�9X@�  @��F@�dZ@�"�@�
=@��@��y@��@���@�=q@���@��^@���@�p�@�?}@���@��u@�9X@��@��@��R@�^5@�V@�=q@��@���@�/@��9@�j@�I�@� �@�  @���@�\)@�o@���@��@��@��!@�M�@��T@�J@�$�@�@��#@���@�7L@���@�Ĝ@��j@��9@��9@��u@�;@~�R@}��@}`B@|�D@|I�@|�@{�
@{��@{C�@z�@z^5@zJ@y�#@yx�@y&�@xbN@w
=@v�y@vȴ@vff@uO�@t�@tZ@s��@s33@r��@r=q@q��@pĜ@p�u@pr�@pQ�@p  @ol�@nȴ@nff@n$�@m�T@m�T@m�h@l��@l�/@l�j@lj@l1@kƨ@kt�@j�H@j^5@i�@i��@ix�@iX@i&�@hbN@g�@g|�@f�R@fV@f{@e��@e`B@e/@d�j@dZ@dI�@d�@c��@c�@c33@co@b��@a��@a�7@a�7@a7L@`��@`Q�@_��@^ȴ@^$�@]�@]��@]��@]/@\�/@\1@[��@[dZ@Z�\@Z-@Z�@Yx�@X��@X�u@X1'@W�@WK�@W�@V��@V5?@U��@UO�@T�j@T9X@S�m@SdZ@S33@R�H@R�\@R=q@Q��@Q�@P��@P��@Pr�@O�@OK�@N��@Nff@NV@N$�@M�T@M�@M/@L��@Lj@L1@KdZ@J~�@JM�@J=q@I��@I��@I�^@I��@I�7@I7L@H��@H�9@H��@H�@HA�@G��@G|�@GK�@F��@Fff@FE�@F{@E@E@E�-@E�-@E�-@E��@E�@D��@D9X@C��@C��@CC�@B�H@B��@B~�@BJ@Ahs@A&�@A%@A%@@��@@�`@@��@@ �@?�;@?��@?�w@?�@>�+@>V@=��@=?}@=�@=�@<��@<��@<j@<(�@<1@;��@;��@:�H@:n�@:=q@:�@9�@9G�@8bN@7�;@7�@7��@7\)@6��@6��@6��@6V@5��@5�@5p�@5p�@4�@4I�@41@3�
@333@3@2��@2�!@2�\@1��@1X@0��@0��@0r�@/�@/\)@/
=@.ȴ@.�+@.E�@.{@.@-��@-�@-?}@,�@,�D@,1@+��@+t�@+S�@+o@*�H@*��@*^5@*J@)��@)x�@)x�@)hs@(��@(r�@(A�@( �@'�@'|�@'�P@'|�@'l�@'�@&��@&�y@&��@%�@%�@%�@$�j@$�D@$z�@$Z@#�m@#��@#t�@#dZ@#33@#o@#o@#@"�@"�\@!�@!��@!�7@!%111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  A��A���Aҟ�AҍPA�r�A��A�A���A��A��A��yA��`A��TA��HA��HA��/A��#A��A��A���A���A���A���A�ƨAѸRAэPA�`BA� �AЋDA��A�/A�ZA�A�(�A�$�A�$�A��A��TA���A�E�A�XA��A��mA�^5A���A�p�A�+A���A�ȴA�bNA��A�l�A��A���A���A�7LA�S�A�1'A�?}A��RA���A�bA�VA�^5A���A���A��A�z�A��DA�-A�|�A��A���A�
=A��+A��;A��yA���A�|�A��A��wA}��Ay�mAtffAr�uAl��Ai�wAgt�Ad^5AaAZ��AUdZARI�AN�uALz�AJE�AH��AE&�ABȴAA\)A?��A<��A:=qA7��A5�hA4�A3�A1dZA/�A.r�A-��A,�jA,n�A+dZA+?}A*��A*-A)�A'��A&�HA&�A&(�A#�A$Q�A%A&�A&��A'�A'/A'S�A'+A'A&ĜA%l�A#�A"�\A"E�A"(�A!��A!/A ��A�A��A;dAA�An�A��A��Al�A�TAVAbA�`A�\A1'A�A��A|�AS�A�jA�7A�AoA�AA�A��A|�A��A��AM�A�FA33A1'A��A��AC�A��A9XA�TA�wA�A?}A�A�/A��A5?A �A�#A��AK�A��A�/A��Az�AQ�A  AƨA?}A�AbA�AK�A
�A
r�A	�A	�wA	��A	p�A	?}A	%A�`AĜAM�A�AJA��AO�A�AȴA-A�TA��A�AC�A%A�HA��AA�A��A�^At�A
=AĜAv�AA�A��AXA%A ��A =q@��@�@�=q@��h@��@��D@�ƨ@��@��R@�n�@�p�@�j@�+@���@��-@�p�@�?}@���@�Ĝ@�A�@���@�"�@�^5@�X@�1'@�"�@���@�-@���@��@�Z@�@��y@�{@�V@�j@���@�dZ@���@�!@�+@��@��@�z�@���@�F@�
=@�+@�5?@���@�x�@�7L@�%@���@��u@�K�@��@ް!@ޏ\@�v�@�n�@�@ݙ�@�?}@��@�z�@��@ۍP@�dZ@�\)@��@�E�@�@���@�r�@��m@�K�@�^5@��`@�Z@�9X@�b@ӥ�@ҏ\@Ѳ-@��@Гu@�A�@�33@�M�@ͩ�@��`@�r�@��m@˕�@�l�@�o@���@�`B@��@�A�@ǅ@�\)@Ƨ�@��@��@���@ź^@�X@ě�@���@�"�@�=q@��#@�p�@��@��u@�bN@�9X@��@�S�@�33@��y@�-@��#@��-@�%@�Q�@��@��@���@�|�@�S�@�@���@��+@�@�&�@��9@�Z@��@���@��y@��R@�v�@��7@�7L@���@�z�@���@�;d@�
=@�@���@���@�V@��-@�x�@�V@�1'@��w@���@�;d@�E�@�@�x�@�%@��/@�Q�@���@��;@���@�l�@�l�@�\)@�33@��@�~�@�V@�=q@�-@��@�G�@��`@�1'@�S�@��@��\@�E�@��^@���@��7@�x�@�X@��@�%@���@��u@�A�@�1@���@�ƨ@�
=@�n�@�5?@��-@�/@��@�z�@�j@�1'@��m@��@�+@��H@��R@��\@�^5@��@�X@��@��@�Ĝ@��9@��@�j@�9X@��m@�ƨ@�t�@�;d@�"�@���@��!@�-@��@���@�&�@��@���@�j@�(�@���@�t�@�C�@�+@��y@��\@�V@�-@��@��-@�hs@�7L@���@��@�r�@�I�@�b@���@�l�@��@�ff@�{@��@���@�?}@��/@��9@�(�@���@��@��;@��F@���@���@�t�@�;d@��H@���@�~�@�n�@�n�@�ff@�-@��-@�x�@��@��u@�bN@�9X@�  @��F@�dZ@�"�@�
=@��@��y@��@���@�=q@���@��^@���@�p�@�?}@���@��u@�9X@��@��@��R@�^5@�V@�=q@��@���@�/@��9@�j@�I�@� �@�  @���@�\)@�o@���@��@��@��!@�M�@��T@�J@�$�@�@��#@���@�7L@���@�Ĝ@��j@��9@��9@��u@�;@~�R@}��@}`B@|�D@|I�@|�@{�
@{��@{C�@z�@z^5@zJ@y�#@yx�@y&�@xbN@w
=@v�y@vȴ@vff@uO�@t�@tZ@s��@s33@r��@r=q@q��@pĜ@p�u@pr�@pQ�@p  @ol�@nȴ@nff@n$�@m�T@m�T@m�h@l��@l�/@l�j@lj@l1@kƨ@kt�@j�H@j^5@i�@i��@ix�@iX@i&�@hbN@g�@g|�@f�R@fV@f{@e��@e`B@e/@d�j@dZ@dI�@d�@c��@c�@c33@co@b��@a��@a�7@a�7@a7L@`��@`Q�@_��@^ȴ@^$�@]�@]��@]��@]/@\�/@\1@[��@[dZ@Z�\@Z-@Z�@Yx�@X��@X�u@X1'@W�@WK�@W�@V��@V5?@U��@UO�@T�j@T9X@S�m@SdZ@S33@R�H@R�\@R=q@Q��@Q�@P��@P��@Pr�@O�@OK�@N��@Nff@NV@N$�@M�T@M�@M/@L��@Lj@L1@KdZ@J~�@JM�@J=q@I��@I��@I�^@I��@I�7@I7L@H��@H�9@H��@H�@HA�@G��@G|�@GK�@F��@Fff@FE�@F{@E@E@E�-@E�-@E�-@E��@E�@D��@D9X@C��@C��@CC�@B�H@B��@B~�@BJ@Ahs@A&�@A%@A%@@��@@�`@@��@@ �@?�;@?��@?�w@?�@>�+@>V@=��@=?}@=�@=�@<��@<��@<j@<(�@<1@;��@;��@:�H@:n�@:=q@:�@9�@9G�@8bN@7�;@7�@7��@7\)@6��@6��@6��@6V@5��@5�@5p�@5p�@4�@4I�@41@3�
@333@3@2��@2�!@2�\@1��@1X@0��@0��@0r�@/�@/\)@/
=@.ȴ@.�+@.E�@.{@.@-��@-�@-?}@,�@,�D@,1@+��@+t�@+S�@+o@*�H@*��@*^5@*J@)��@)x�@)x�@)hs@(��@(r�@(A�@( �@'�@'|�@'�P@'|�@'l�@'�@&��@&�y@&��@%�@%�@%�@$�j@$�D@$z�@$Z@#�m@#��@#t�@#dZ@#33@#o@#o@#@"�@"�\@!�@!��@!�7@!%111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  ;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oB
�?B
�?B
�9B
�9B
�3B
�3B
�3B
�3B
�3B
�3B
�3B
�3B
�3B
�3B
�3B
�3B
�3B
�3B
�3B
�3B
�-B
�-B
�-B
�'B
�!B
�B
��B
��B
��B
�hB
}�B
x�B
�+B
�VB
��B8RB�oB��B��B��B�FB�ZBoB{B%�B)�B)�BB�B^5Bl�B�1B�7B�B�B�B� B{�Bz�Bs�BiyBaHBP�BC�B-B �BVB��B�/B�FB�bB� BbNB8RB{B
�TB
��B
�oB
|�B
e`B
49B
oB	�BB	��B	��B	�VB	m�B	YB	K�B	:^B	'�B	VB��B�yB�ZB�ZB�sB�B�B�B�B�ZB�B��BĜBB�wB�dB�^B�dB�dBB�#B�B��B	1B	�B	{B	hB	
=B	+B	�B	"�B	#�B	49B	_;B	{�B	�1B	�{B	��B	��B	�B	�3B	�dB	�dB	�B	�B	�XB	B	B	��B	�wB	��B	ÖB	�/B	�BB	�NB	�NB	�TB	�TB	�mB	�B
  B
B
B

=B

=B
JB
JB
DB
DB
	7B
	7B
JB
bB
bB
\B
\B
VB
VB
JB
JB
JB
DB
PB
oB
{B
oB
VB
DB
DB
uB
{B
{B
�B
�B
�B
�B
�B
�B
�B
"�B
%�B
'�B
)�B
)�B
)�B
+B
)�B
(�B
+B
.B
.B
.B
/B
.B
.B
.B
0!B
0!B
1'B
33B
33B
33B
33B
33B
49B
49B
33B
2-B
1'B
/B
0!B
1'B
1'B
1'B
1'B
1'B
0!B
0!B
0!B
0!B
/B
.B
-B
-B
,B
)�B
(�B
'�B
&�B
%�B
$�B
#�B
#�B
"�B
"�B
!�B
!�B
"�B
#�B
#�B
!�B
 �B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
{B
uB
oB
oB
oB
hB
oB
oB
hB
hB
hB
bB
hB
bB
bB
\B
VB
VB
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
VB
VB
PB
DB
DB
DB
DB

=B
DB
DB
DB

=B
	7B
	7B
	7B
	7B
	7B
	7B
1B
1B
	7B
	7B
	7B
1B
1B
1B
	7B
	7B
1B
	7B

=B
	7B
1B
+B
+B
+B
+B
+B
+B
+B
%B
+B
+B
+B
+B
+B
+B
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
1B
1B
1B
1B
1B
1B
	7B
	7B
	7B
	7B
	7B
	7B
	7B
	7B
	7B
	7B
	7B
	7B
	7B
1B
	7B

=B

=B

=B

=B
DB
DB
DB
DB
JB
DB
JB
JB
PB
PB
PB
PB
JB
PB
PB
PB
PB
VB
VB
VB
VB
\B
\B
\B
\B
\B
\B
\B
\B
bB
bB
bB
bB
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
hB
hB
hB
hB
oB
oB
oB
oB
oB
oB
oB
oB
oB
uB
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
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
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
 �B
!�B
!�B
!�B
!�B
!�B
"�B
"�B
!�B
!�B
"�B
#�B
$�B
$�B
$�B
$�B
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
%�B
%�B
%�B
&�B
&�B
&�B
&�B
&�B
&�B
&�B
%�B
%�B
&�B
'�B
'�B
(�B
)�B
+B
+B
+B
+B
)�B
)�B
)�B
(�B
(�B
(�B
(�B
+B
,B
,B
+B
)�B
)�B
+B
,B
,B
,B
+B
+B
+B
,B
-B
-B
-B
-B
-B
.B
/B
0!B
2-B
33B
33B
1'B
1'B
2-B
2-B
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
7LB
7LB
7LB
7LB
7LB
7LB
9XB
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
;dB
;dB
<jB
<jB
<jB
=qB
=qB
=qB
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
@�B
@�B
A�B
A�B
A�B
A�B
A�B
A�B
A�B
B�B
B�B
B�B
B�B
C�B
B�B
B�B
C�B
C�B
C�B
D�B
D�B
D�B
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
F�B
F�B
F�B
G�B
G�B
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
I�B
I�B
I�B
J�B
J�B
J�B
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
N�B
O�B
O�B
O�B
O�B
O�B
P�B
P�B
P�B
P�B
P�B
Q�B
Q�B
Q�B
R�B
R�B
R�B
R�B
R�B
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
`BB
`BB
`BB
`BB
aHB
aHB
aHB
aHB
bNB
cTB
cTB
bNB
cTB
cTB
dZB
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
iyB
iyB
iyB
jB
jB
jB
jB
k�B
l�B
k�B
k�B
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
m�B
m�B
n�B
m�B
m�B
m�B
m�B
n�B
n�B
n�B
n�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  B
��B
��B
��B
��B
��B
��B
�eB
�kB
�QB
�QB
�QB
�MB
�KB
�EB
�OB
�FB
�JB
�?B
�MB
�PB
�CB
�CB
�UB
�jB
��B
��B
��B
��B
�\B
�~B
��B
|B
��B
�\B
��B=|B�EB��B�B��B��B�B�BLB*�B/7B0hBFB_�BnxB��B�B�>B�/B��B��B|dB}�Bx�Bl�BfxBTsBI�B/�B$�B�B~B�B��B�NB��Bl�B>�B�B
�VB
�eB
�ZB
��B
s:B
<�B
"�B	�yB	�B	��B	�_B	r�B	\�B	P�B	?�B	2}B	UB�LB�B� B�B�QB��B�aB�GB�1B�B�MBԋB��B�ZB�MB�B�)B�#B��B��B�B�[B�IB		[B	|B	kB	�B	&B	.B	�B	'PB	#B	1FB	]�B	{UB	��B	�hB	�jB	�FB	��B	��B	��B	�B	�tB	��B	��B	��B	íB	��B	�1B	��B	�B	��B	�3B	�B	�B	�WB	��B	�*B	�B
 �B
�B
1B
kB
$B
?B
�B
�B
AB
#B

�B
vB
AB
TB
�B
�B
�B
B
�B
@B
B
hB
B
�B
�B
TB
�B
KB
�B
8B
@B
B
8B
MB
�B
�B
TB
]B
}B
#�B
&EB
(�B
*dB
*vB
*�B
+�B
+B
)�B
,'B
.�B
.�B
.�B
02B
/*B
.�B
.VB
0�B
0�B
1�B
3�B
3�B
4VB
3�B
3sB
4�B
5�B
3�B
3"B
2�B
/�B
0bB
2
B
1�B
1�B
1�B
2 B
1B
0�B
0�B
0�B
0=B
.�B
-�B
-�B
-MB
+3B
)�B
(�B
(mB
'0B
%�B
$�B
$�B
#�B
#�B
"�B
"�B
#.B
$UB
%/B
#4B
"lB
 �B
 B
B
B
B
B
lB
MB
�B
�B
B
.B

B
%B
eB
B
�B
�B
wB
�B
�B
�B
`B
MB
B
JB
�B
�B
)B
�B
�B
�B
�B
 B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
uB
�B
�B
�B
�B
�B
�B
B
�B
bB
�B
1B
	�B

RB
	�B

 B

$B

�B

=B
�B
	uB
	�B
	�B
	�B
	fB
	MB
	�B
	�B
	�B

uB
+B

KB
�B
�B
�B
wB
�B
�B
�B
�B
B
'B
yB
B
�B
~B
[B
aB
�B
.B
BB
	,B
	pB
	�B
	�B
	�B
	�B
	�B
yB
�B
�B
tB
�B
	6B
	�B
	�B

/B

1B
	�B
	�B
	�B
	�B
	�B
	�B
	�B
	}B

B
	sB
	�B

�B

�B
B
HB
�B
�B
�B
�B
�B
�B
JB
�B
�B
nB
kB
�B
�B
B
�B
�B
^B
�B
�B
�B
�B
B
�B
�B
�B
B
�B
�B
�B
�B
uB
�B
�B
�B
�B
�B
�B
�B
�B
YB
�B
aB
�B
B
B
�B
9B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
]B
�B
@B
<B
�B
/B
�B
�B
�B
"B
B
 B
�B
�B
�B
0B
bB
�B
�B
�B
�B
�B
�B
�B
$B
�B
+B

B
�B
�B
$B
iB
B
5B
XB
B
9B
B
'B
}B
 B
 B
�B
 +B
 IB
!)B
!B
!1B
!)B
!>B
"&B
"6B
"MB
".B
"B
#9B
#B
"�B
"�B
#�B
$hB
%#B
%`B
%�B
%|B
%)B
%�B
&)B
& B
&B
&(B
&	B
&B
&'B
&<B
&`B
&:B
&+B
'B
&�B
'B
'DB
'�B
'FB
'�B
&hB
&:B
'1B
(MB
(fB
)mB
*aB
+4B
+3B
+!B
+*B
*]B
*�B
*dB
)^B
)5B
)@B
)PB
+jB
,�B
,�B
+{B
*�B
+ B
+�B
,'B
,@B
,{B
+�B
+�B
+�B
,zB
-NB
-ZB
-MB
-jB
-�B
.�B
/QB
0=B
2dB
3�B
3�B
1�B
0�B
2 B
2rB
3�B
3fB
5B
4�B
5�B
5\B
5`B
5WB
5�B
6GB
76B
7B
7�B
7�B
7�B
7�B
7�B
7�B
9�B
9�B
9�B
9�B
:�B
:�B
:�B
;B
;tB
;�B
;�B
;�B
<SB
<�B
<�B
=B
=�B
=�B
=�B
?
B
?@B
?�B
?�B
?�B
?�B
@B
@B
@�B
@�B
@�B
@�B
@�B
AB
A�B
A�B
A�B
A�B
A�B
A�B
BB
CB
B�B
B�B
B�B
C�B
B�B
CB
C�B
C�B
D$B
D�B
D�B
D�B
E�B
E�B
E�B
F�B
F�B
F�B
F�B
G B
F�B
F�B
F�B
G=B
GB
G�B
G�B
HB
HB
H9B
HKB
I.B
H�B
H�B
H�B
IB
JB
JQB
JB
I�B
JXB
KB
J�B
K@B
L5B
L$B
L!B
L3B
M'B
MB
M0B
M2B
M$B
NAB
NQB
NHB
N#B
OHB
PB
P,B
P-B
P4B
PMB
QxB
Q,B
QB
Q B
Q[B
RxB
RB
R$B
SB
S*B
S:B
SKB
SEB
SLB
SOB
SSB
S�B
T�B
T2B
TB
T;B
U5B
UB
UB
U+B
USB
UFB
U?B
UB
U-B
UIB
V�B
V1B
VAB
V�B
WPB
W9B
WDB
W]B
WB
W'B
W!B
W B
W*B
W?B
W�B
X�B
X[B
XuB
YoB
YvB
Y[B
YGB
Y}B
Z�B
ZYB
ZHB
[6B
[?B
[=B
[^B
[�B
[_B
[AB
[CB
[�B
\�B
\^B
\�B
\�B
]XB
]GB
]]B
]�B
]dB
]mB
^]B
^UB
^�B
^�B
^�B
_nB
_dB
_pB
_�B
_�B
`�B
`vB
`gB
`�B
`�B
`�B
`bB
`�B
`�B
a�B
agB
a`B
a�B
b�B
c�B
c�B
b�B
c�B
c�B
d�B
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
i�B
i�B
i�B
j�B
j�B
j�B
j�B
k�B
l�B
k�B
l+B
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
m�B
m�B
n�B
m�B
m�B
m�B
nB
n�B
n�B
n�B
n�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  <#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<�t�<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<,�R<#�
<#�
<;i<#�
<GЖ<�t<#�
<#�
<#�
<lG�<#�
<��G<#�
<#�
<#�
<&)<#�
<#�
<#�
<#�
<;��<3�X<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
PRES            TEMP            PSAL            PRES_ADJUSTED = PRES - (REPORTED_SURFACE_PRESSURE) for PADJ                                                                                                                                                                                                     none                                                                                                                                                                                                                                                            PSAL_ADJ corrects Cnd. Thermal Mass (CTM), Johnson et al,2007,JAOT & effects of pressure adjustments                                                                                                                                                            PADJ REPORTED_SURFACE_PRESSURE =0.15 dbar                                                                                                                                                                                                                       none                                                                                                                                                                                                                                                            CTM alpha = 0.141 & tau = 6.68 s with error equal to the correction                                                                                                                                                                                             Pressures adjusted using reported surface pressure when warranted.                                                                                                                                                                                              none                                                                                                                                                                                                                                                            Salinity corrected for pressure sensor calibration drift and cell thermal mass                                                                                                                                                                                  201810180917402018101809174020181018091740  0188                            052512                          AO  ARCAADJP                                                                    20140722011108    IP                G�O�G�O�G�O�                AO  ARGQQCPL                                                                    20140722011108  QCP$                G�O�G�O�G�O�8FB7E           AO  ARGQQCPL                                                                    20140722011108  QCF$                G�O�G�O�G�O�0                                                                                                                                   G�O�G�O�G�O�                PM  ARSQPADJV1.1                                                                20181018091740  QC  PRES            @�ffDٶfG�O�                PM  ARSQCTM V1.1                                                                20181018091740  QC  PSAL            @�ffDٶfG�O�                PM  ARSQOWGUV1.0                                                                20181023142215  IP                  G�O�G�O�G�O�                