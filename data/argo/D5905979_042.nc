CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             
   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       2020-06-19T17:09:03Z creation      
references        (http://www.argodatamgt.org/Documentation   comment       	free text      user_manual_version       3.2    Conventions       Argo-3.2 CF-1.6    featureType       trajectoryProfile      comment_dmqc_operator         ZPRIMARY | https://orcid.org/0000-0001-7324-3159 | Matthew Alkire, University of Washington        @   	DATA_TYPE                  	long_name         	Data type      conventions       Argo reference table 1     
_FillValue                    7   FORMAT_VERSION                 	long_name         File format version    
_FillValue                    7(   HANDBOOK_VERSION               	long_name         Data handbook version      
_FillValue                    7,   REFERENCE_DATE_TIME                 	long_name         !Date of reference for Julian days      conventions       YYYYMMDDHHMISS     
_FillValue                    70   DATE_CREATION                   	long_name         Date of file creation      conventions       YYYYMMDDHHMISS     
_FillValue                    7@   DATE_UPDATE                 	long_name         Date of update of this file    conventions       YYYYMMDDHHMISS     
_FillValue                    7P   PLATFORM_NUMBER                   	long_name         Float unique identifier    conventions       WMO float identifier : A9IIIII     
_FillValue                    7`   PROJECT_NAME                  	long_name         Name of the project    
_FillValue                  @  7h   PI_NAME                   	long_name         "Name of the principal investigator     
_FillValue                  @  7�   STATION_PARAMETERS           	            	long_name         ,List of available parameters for the station   conventions       Argo reference table 3     
_FillValue                  0  7�   CYCLE_NUMBER               	long_name         Float cycle number     conventions       =0...N, 0 : launch cycle (if exists), 1 : first complete cycle      
_FillValue         ��        8   	DIRECTION                  	long_name         !Direction of the station profiles      conventions       -A: ascending profiles, D: descending profiles      
_FillValue                    8   DATA_CENTRE                   	long_name         .Data centre in charge of float data processing     conventions       Argo reference table 4     
_FillValue                    8    DC_REFERENCE                  	long_name         (Station unique identifier in data centre   conventions       Data centre convention     
_FillValue                     8$   DATA_STATE_INDICATOR                  	long_name         1Degree of processing the data have passed through      conventions       Argo reference table 6     
_FillValue                    8D   	DATA_MODE                  	long_name         Delayed mode or real time data     conventions       >R : real time; D : delayed mode; A : real time with adjustment     
_FillValue                    8H   PLATFORM_TYPE                     	long_name         Type of float      conventions       Argo reference table 23    
_FillValue                     8L   FLOAT_SERIAL_NO                   	long_name         Serial number of the float     
_FillValue                     8l   FIRMWARE_VERSION                  	long_name         Instrument firmware version    
_FillValue                     8�   WMO_INST_TYPE                     	long_name         Coded instrument type      conventions       Argo reference table 8     
_FillValue                    8�   JULD               	long_name         ?Julian day (UTC) of the station relative to REFERENCE_DATE_TIME    standard_name         time   units         "days since 1950-01-01 00:00:00 UTC     conventions       8Relative julian days with decimal part (as parts of day)   
resolution        >�EȠ      
_FillValue        A.�~       axis      T           8�   JULD_QC                	long_name         Quality on date and time   conventions       Argo reference table 2     
_FillValue                    8�   JULD_LOCATION                  	long_name         @Julian day (UTC) of the location relative to REFERENCE_DATE_TIME   units         "days since 1950-01-01 00:00:00 UTC     conventions       8Relative julian days with decimal part (as parts of day)   
resolution        >�EȠ      
_FillValue        A.�~            8�   LATITUDE               	long_name         &Latitude of the station, best estimate     standard_name         latitude   units         degree_north   
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
resolution        =���   axis      Z        �  9�   PRES_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                    A�   PRES_ADJUSTED            
      	   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  C�   PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                    K�   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         decibar    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  M�   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  U�   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                    ]�   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  _�   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                    g�   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         degree_Celsius     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  i�   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  q�   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                    y�   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  {�   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                    ��   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  ��   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  0  ��   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                    ��   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                    ��   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                    ��   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  ,  ��   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                    �   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                    �   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                    �   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                    �   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  @  �   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                    �T   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                    �d   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    �h   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar         �x   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar         �|   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�        ��   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    ��Argo profile    3.1 1.2 19500101000000  20200619170903  20220204114414  5905979 UW, Argo                                                        STEPHEN RISER                                                   PRES            TEMP            PSAL               *A   AO  7662                            2C  D   APEX                            8312                            080318                          846 @ؔ�	)1   @ؔ)� <@7��O�;d�c�I�^51   GPS     Primary sampling: mixed [deeper than nominal 985dbar: discrete; nominal 985dbar to surface: 2dbar-bin averaged]                                                                                                                                                    *A   B   B   @���@���A   A!��AA��A`  A~ffA�  A�  A�  A�  A�  A�  A�  B   B  B  B  B   B(  B0  B8  B@  BH  BP  BW��B`  Bh  Bp  Bx  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B���B�  B�  B�  B�  B�  B�  C   C  C  C  C  C
  C  C  C  C  C  C  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D��D� D  D� DfD� D  D� D	  D	� D
  D
� D  D� D  D� D  D� DfD� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'fD'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9�fD:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DEy�DF  DF�fDG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DNy�DO  DO� DP  DPy�DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dt� Dt�3Dy�D�{D�T{D��HD��D�!�D�\{D���D���D��D�YHD��Dǿ\D��D�O
Dڬ)D��3D��D�Q�D�D���111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  @���@�  @�33A34A?34A]��A|  A���A���A���A���A���A���A���A���BffBffBffBffB'ffB/ffB7ffB?ffBGffBOffBW  B_ffBgffBoffBwffBffB��3B��3B��3B��3B��3B��3B��3B��3B��3B��3B��3B��3B��3B��3B��3B��3Bó3Bǳ3B˳3Bϳ3Bӳ3B׳3B۳3B߳3B� B�3B�3B�3B�3B��3B��3B��3CٚCٚCٚCٚC	ٚCٚCٚCٚCٚCٚCٚCٚCٚCٚCٚCٚC!ٚC#ٚC%ٚC'ٚC)ٚC+ٚC-ٚC/ٚC1ٚC3ٚC5ٚC7ٚC9ٚC;ٚC=ٚC?ٚCAٚCCٚCEٚCGٚCIٚCKٚCMٚCOٚCQٚCSٚCUٚCWٚCYٚC[ٚC]ٚC_ٚCaٚCcٚCeٚCgٚCiٚCkٚCmٚCoٚCqٚCsٚCuٚCwٚCyٚC{ٚC}ٚCٚC���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C�� C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���D vfD �fDvfD�fDvfD�fDvfD�fDvfD� DvfD�fDvfD��DvfD�fDvfD�fD	vfD	�fD
vfD
�fDvfD�fDvfD�fDvfD��DvfD�fDvfD�fDvfD�fDvfD�fDvfD�fDvfD�fDvfD�fDvfD�fDvfD�fDvfD�fDvfD�fDvfD�fDvfD�fDvfD�fDvfD�fDvfD�fDvfD�fDvfD�fD vfD �fD!vfD!�fD"vfD"�fD#vfD#�fD$vfD$�fD%vfD%�fD&vfD&��D'vfD'�fD(vfD(�fD)vfD)�fD*vfD*�fD+vfD+�fD,vfD,�fD-vfD-�fD.vfD.�fD/vfD/�fD0vfD0�fD1vfD1�fD2vfD2�fD3vfD3�fD4vfD4�fD5vfD5�fD6vfD6�fD7vfD7�fD8vfD8�fD9|�D9�fD:vfD:�fD;vfD;�fD<vfD<�fD=vfD=�fD>vfD>�fD?vfD?�fD@vfD@�fDAvfDA�fDBvfDB�fDCvfDC�fDDvfDD�fDEp DE�fDF|�DF�fDGvfDG�fDHvfDH�fDIvfDI�fDJvfDJ�fDKvfDK�fDLvfDL�fDMvfDM�fDNp DN�fDOvfDO�fDPp DP�fDQvfDQ�fDRvfDR�fDSvfDS�fDTvfDT�fDUvfDU�fDVvfDV�fDWvfDW�fDXvfDX�fDYvfDY�fDZvfDZ�fD[vfD[�fD\vfD\�fD]vfD]�fD^vfD^�fD_vfD_�fD`vfD`�fDavfDa�fDbvfDb�fDcvfDc�fDdvfDd�fDevfDe�fDfvfDf�fDgvfDg�fDhvfDh�fDivfDi�fDjvfDj�fDkvfDk�fDlvfDl�fDmvfDm�fDnvfDn�fDovfDo�fDpvfDp�fDqvfDq�fDrvfDr�fDsvfDs�fDtvfDtəDyGD��D�O�D��{D��RD�D�W�D���D��D� D�T{D��GDǺ�D�D�J=Dڧ\D��fD� D�L�D�~�D��111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  @��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��A��TA��TA��`A��`A��TA��/A��HA��`A��A��A��A��A��A�  A�  A�  A�  A�A�A�A�A�1A�JA�JA�JA�VA�bA�oA�oA�JA�JA�1A�A�A���A���A���A��HAɋDA�+A�^5A�r�A���A��A�bA��A�K�A�p�A�9XA�(�A�%A���A�t�A���A���A���A��\A���A��A���A���A�ȴA�ZA���A��A�-A��A�ZA�ƨA�l�A�/A���A��A�ffA�A���A�M�A�
=A�33A��A�A�A� �A�ȴA��A�C�A���A��HA���A�
=A�r�A���A�(�A�ƨA�E�A��A�ȴA�t�A��A��A��A�oA�G�A��yA�?}A�"�A���A��FA���A���A���A��/A�r�A��A�bNA�"�A��9A��A���A�r�A�XA�`BA�A��A~�yA}�hA{��Azv�Ax��AudZAq��AodZAn�An�uAm��Al�\Ai�#Ag��Ad�yAc7LA^AZZAY��AX��AV��AT�yAS�PARz�AQ��AQ�FAO�#AN��AL~�AJ��AJQ�AI��AIhsAH��AF�/AES�AC?}A@�A@1'A?C�A=�A<ȴA;7LA:��A8��A7��A5��A4z�A2Q�A1l�A0z�A/%A-l�A,M�A+�A+VA*z�A*1'A)A((�A'A%�A$jA#�A#�A"��A!�A!��A =qAt�A�DA
=A�jAffA  A�A��A��A��A�PA%A-A|�A�`A�!Ar�A9XA\)A�uA$�A�mAG�A�/A~�A�PAG�A��A�AA��A
A�A	
=A(�At�AI�A`BA��A1A��A�A z�@�\)@��@��@�~�@���@��@�X@�X@�9X@�5?@�@�P@�M�@���@�1'@�ƨ@��@�`B@ܣ�@�  @�\)@�~�@�V@ָR@��T@���@���@�o@д9@ϝ�@�^5@̋D@���@��@�v�@���@ȓu@�K�@�V@�`B@ēu@���@öF@Õ�@�+@�@��@�r�@�dZ@��R@�~�@���@���@��/@��9@�Z@�K�@���@��+@�5?@��^@���@�I�@� �@�Q�@�b@�o@��@�C�@�K�@�C�@���@�$�@���@�I�@��F@�\)@�@�M�@�$�@���@�?}@���@��D@�A�@��@���@�=q@���@���@�ƨ@�l�@��@�M�@�@��7@�X@���@��j@�r�@��@��w@���@���@��@��@�bN@�1@��@���@�V@��T@�7L@��@��@��\@�E�@��@�o@�o@�@�@���@�\)@�K�@�+@���@��@�V@��@�;d@�o@�ȴ@�33@�o@�+@��R@�^5@�V@��T@�?}@��@���@� �@�1@�S�@��@�^5@�?}@�/@��`@�Q�@�bN@�A�@��@�1@���@��@��@�dZ@�l�@�t�@�|�@�|�@�C�@�~�@�ff@�x�@��j@�b@��@�J@���@�?}@�hs@�&�@��`@��@��@�ƨ@�n�@�$�@��#@�p�@�O�@�/@��@�?}@�p�@���@�X@��@��@�Ĝ@���@�1'@�  @�Q�@��m@��@���@�n�@��!@���@�-@��#@���@�%@���@��@��H@���@��+@�=q@�J@�@��@��T@�@���@�x�@��`@�Q�@�Z@�b@��;@�b@��`@��D@��@��F@�33@�+@�K�@�t�@��P@�|�@�t�@�J@��@���@�V@��`@��@��`@���@��@���@��9@��`@�I�@���@�C�@��y@�ȴ@��@~�,@p��@ip�@a�-@W�r@P�@G�	@A-w@;ƨ@7�@3]�@-�T@&u%@!s�@Y�@�@/@��@[W@	8�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111  A��TA��TA��`A��`A��TA��/A��HA��`A��A��A��A��A��A�  A�  A�  A�  A�A�A�A�A�1A�JA�JA�JA�VA�bA�oA�oA�JA�JA�1A�A�A���A���A���A��HAɋDA�+A�^5A�r�A���A��A�bA��A�K�A�p�A�9XA�(�A�%A���A�t�A���A���A���A��\A���A��A���A���A�ȴA�ZA���A��A�-A��A�ZA�ƨA�l�A�/A���A��A�ffA�A���A�M�A�
=A�33A��A�A�A� �A�ȴA��A�C�A���A��HA���A�
=A�r�A���A�(�A�ƨA�E�A��A�ȴA�t�A��A��A��A�oA�G�A��yA�?}A�"�A���A��FA���A���A���A��/A�r�A��A�bNA�"�A��9A��A���A�r�A�XA�`BA�A��A~�yA}�hA{��Azv�Ax��AudZAq��AodZAn�An�uAm��Al�\Ai�#Ag��Ad�yAc7LA^AZZAY��AX��AV��AT�yAS�PARz�AQ��AQ�FAO�#AN��AL~�AJ��AJQ�AI��AIhsAH��AF�/AES�AC?}A@�A@1'A?C�A=�A<ȴA;7LA:��A8��A7��A5��A4z�A2Q�A1l�A0z�A/%A-l�A,M�A+�A+VA*z�A*1'A)A((�A'A%�A$jA#�A#�A"��A!�A!��A =qAt�A�DA
=A�jAffA  A�A��A��A��A�PA%A-A|�A�`A�!Ar�A9XA\)A�uA$�A�mAG�A�/A~�A�PAG�A��A�AA��A
A�A	
=A(�At�AI�A`BA��A1A��A�A z�@�\)@��@��@�~�@���@��@�X@�X@�9X@�5?@�@�P@�M�@���@�1'@�ƨ@��@�`B@ܣ�@�  @�\)@�~�@�V@ָR@��T@���@���@�o@д9@ϝ�@�^5@̋D@���@��@�v�@���@ȓu@�K�@�V@�`B@ēu@���@öF@Õ�@�+@�@��@�r�@�dZ@��R@�~�@���@���@��/@��9@�Z@�K�@���@��+@�5?@��^@���@�I�@� �@�Q�@�b@�o@��@�C�@�K�@�C�@���@�$�@���@�I�@��F@�\)@�@�M�@�$�@���@�?}@���@��D@�A�@��@���@�=q@���@���@�ƨ@�l�@��@�M�@�@��7@�X@���@��j@�r�@��@��w@���@���@��@��@�bN@�1@��@���@�V@��T@�7L@��@��@��\@�E�@��@�o@�o@�@�@���@�\)@�K�@�+@���@��@�V@��@�;d@�o@�ȴ@�33@�o@�+@��R@�^5@�V@��T@�?}@��@���@� �@�1@�S�@��@�^5@�?}@�/@��`@�Q�@�bN@�A�@��@�1@���@��@��@�dZ@�l�@�t�@�|�@�|�@�C�@�~�@�ff@�x�@��j@�b@��@�J@���@�?}@�hs@�&�@��`@��@��@�ƨ@�n�@�$�@��#@�p�@�O�@�/@��@�?}@�p�@���@�X@��@��@�Ĝ@���@�1'@�  @�Q�@��m@��@���@�n�@��!@���@�-@��#@���@�%@���@��@��H@���@��+@�=q@�J@�@��@��T@�@���@�x�@��`@�Q�@�Z@�b@��;@�b@��`@��D@��@��F@�33@�+@�K�@�t�@��P@�|�@�t�@�J@��@���@�V@��`@��@��`@���@��@���@��9@��`@�I�@���@�C�@��yG�O�@��@~�,@p��@ip�@a�-@W�r@P�@G�	@A-w@;ƨ@7�@3]�@-�T@&u%@!s�@Y�@�@/@��@[W@	8�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111  ;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oG�O�;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oB[#B[#B[#B[#B[#B[#B[#B[#B[#B[#B[#B[#B[#B\)B\)B\)B\)B\)B[#B[#B[#B\)B^5B^5B^5B^5B^5B^5B^5B`BBaHBdZBffBgmBiyBjBjBk�BhsBffBbNBhsBr�Br�Bq�Bq�Bq�Bu�Bv�By�Bz�Bz�B{�B~�B�B�1B�PB�\B�\B�bB�VB�PB�=B�7B�B�B}�B�B�B{�Bw�Bs�Bo�Bq�Bw�Bx�Bt�Bq�Bk�Be`BaHB_;B\)BS�BI�BA�B8RB)�B�B�B	7BB��B��B�B�`B�;B�B��B�wB�!B��B��B�1Bq�Bk�B^5BQ�BE�B0!B�B
��B
�B
�;B
��B
�wB
�B
��B
��B
�{B
m�B
bNB
_;B
YB
O�B
F�B
:^B
0!B
�B	��B	�ZB	�/B	�B	��B	ȴB	�FB	��B	�oB	~�B	\)B	8RB	6FB	49B	'�B	uB	B��B�B�yB�ZB�
B��BÖB��B�wB�qB�XB�'B�B�!B��B��B��B��B�{B�1B�%B� B{�Bu�Bu�Bl�Bk�BhsBgmBdZBdZBcTBaHB`BB_;B_;B_;B^5B\)B\)B[#B[#B_;B_;BcTBffBbNBbNB]/B\)B[#BZBXBVBT�BO�BN�BM�BL�BK�BI�BI�BI�BI�BI�BJ�BI�BJ�BJ�BI�BI�BH�BG�BE�BF�BD�BC�BC�BB�B@�B@�B@�B?}B>wB>wB=qB;dB;dB9XB9XB7LB7LB7LB9XB=qB=qB<jB>wB?}B?}B?}BA�B@�B@�BB�BB�BB�BA�BA�BA�BC�BD�BD�BE�BE�BG�BJ�BJ�BM�BP�BP�BR�BR�BT�BVBYBZB\)B_;B`BBaHBaHBbNBffBffBk�Bn�Bo�Bo�Bs�Bs�Bt�Bt�Bu�By�B{�B{�B{�B{�By�By�B{�B~�B� B~�B� B�B�B�B�B�1B�DB�JB�PB�VB�\B�\B�\B�oB�{B��B��B��B��B��B��B�B�B�'B�3B�RB�jB��B��BBĜBĜBŢBƨB��B��B��B�
B�TB�B�B�B�B�B��B��B��B��B��B	  B	DB	\B	uB	�B	�B	�B	!�B	"�B	!�B	 �B	#�B	#�B	�B	�B	�B	�B	!�B	+B	1'B	7LB	5?B	5?B	7LB	9XB	:^B	:^B	;dB	J�B	N�B	M�B	M�B	M�B	P�B	R�B	W
B	ZB	[#B	\)B	]/B	\)B	_;B	aHB	bNB	cTB	cTB	cTB	cTB	cTB	bNB	e`B	e`B	bNB	cTB	`BB	]/B	]/B	]/B	_;B	`BB	aHB	cTB	cTB	cTB	aHB	cTB	e`B	ffB	ffB	ffB	ffB	iyB	k�B	o�B	o�B	o�B	p�B	q�B	u�B	v�B	w�B	{�B	~�B	~�B	�B	�\B	�hB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�RB	�qB	��B	��B	B	ƨB	ǮB	��B	��B	��B	��B	��B	ǮB	ɺB	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�B	�B	�B	��B	��B	�"B
	�B
�B
#B
*KB
6B
:�B
A�B
F�B
K^B
P.B
XEB
]~B
a-B
eB
i_B
lqB
q[B
w�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111  BR�BR�BR�BR�BR�BR�BR�BR�BR�BR�BR�BR�BR�BS�BS�BS�BS�BS�BR�BR�BR�BS�BU�BU�BU�BU�BU�BU�BU�BW�BX�B[�B]�B^�BaBbBbBcB`B]�BY�B`Bj@Bj@Bi:Bi:Bi:BmSBnYBqkBrrBrrBsxBv�Bz�B�B��B��B��B��B��B��B��B��B{�Bx�Bu�By�By�BszBobBkIBg2Bi>BobBphBlPBi>BcB\�BX�BV�BS�BK�BAQB9!B/�B!�B;BB �B��B��B�xB�<B��B��BзBʒB�B��B��B�VB�BiRBc-BU�BI�B=MB'�B0B
�B
�[B
��B
șB
�,B
��B
��B
�jB
�4B
eMB
Z
B
V�B
P�B
G�B
>gB
2B
'�B
IB	�B	� B	��B	��B	ʹB	�|B	�B	��B	�;B	v�B	S�B	0%B	.B	,B	�B	KB��B�B�dB�RB�4B��BŮB�rB�eB�TB�NB�5B�B��B��B��B��B��B�sB�\B�B~Bw�Bs�Bm�Bm�BdpBcjB`XB_RB\@B\@B[:BY.BX(BW!BW!BW"BVBTBTBS
BSBW"BW"B[;B^MBZ6BZ6BUBTBSBRBO�BM�BL�BG�BF�BE�BD�BC�BA�BA�BA�BA�BA�BB�BA�BB�BB�BA�BA�B@�B?�B=�B>�B<�B;�B;�B:{B8pB8pB8pB7jB6dB6dB5_B3RB3RB1FB1FB/;B/;B/;B1GB5`B5aB4ZB6gB7mB7mB7mB9yB8sB8sB:B:B:B9yB9yB9yB;�B<�B<�B=�B=�B?�BB�BB�BE�BH�BH�BJ�BJ�BL�BM�BQBRBTBW+BX2BY8BY8BZ>B^VB^VBcuBf�Bg�Bg�Bk�Bk�Bl�Bl�Bm�Bq�Bs�Bs�Bs�Bs�Bq�Bq�Bs�Bv�Bw�Bv�Bw�B|B}B}B}B� B�2B�8B�>B�DB�JB�JB�JB�]B�iB�uB��B��B��B��B��B��B��B�B� B�>B�VB�oB�uB�{B��B��B��B��B¬BòB��B��B�>B�hB�tB�B�B�B�B��B��B��B��B��B	,B	DB	\B	nB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	"�B	)B	/1B	-%B	-%B	/1B	1=B	2CB	2CB	3IB	B�B	F�B	E�B	E�B	E�B	H�B	J�B	N�B	RB	SB	TB	UB	TB	WB	Y+B	Z1B	[7B	[7B	[7B	[7B	[7B	Z1B	]CB	]CB	Z1B	[7B	X&B	UB	UB	UB	WB	X&B	Y,B	[8B	[8B	[8B	Y,B	[8B	]DB	^JB	^JB	^JB	^JB	a]B	chB	g�B	g�B	g�B	h�B	i�B	m�B	n�B	o�B	s�B	v�B	v�B	{�B	�=B	�IB	�bB	�nB	�tB	�nB	�{B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�1B	�PB	�aB	�gB	�mB	��B	��B	B	īB	īB	űB	B	��B	��B	åB	īB	űB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��G�O�B	۴B	�B	��B
aB
�B
�B
"%B
-�B
2�B
9�B
>�B
C7B
HB
PB
UVB
YB
\�B
a7B
dIB
i3B
oq111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111  <#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
G�O�<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
PRES            TEMP            PSAL            PRES_ADJUSTED = PRES - dP                                                                                                                                                                                                                                       none                                                                                                                                                                                                                                                            PSAL_ADJUSTED = PSAL + dS, where dS is calculated from a potential conductivity (ref to 0 dbar) multiplicative adjustment factor r.                                                                                                                             dP =0.15 dbar.                                                                                                                                                                                                                                                  none                                                                                                                                                                                                                                                            r =0.9998(+/-0.0001), vertically averaged dS =-0.008(+/-0.002) in PSS-78.                                                                                                                                                                                       Pressures adjusted by using pressure offset at the sea surface. The quoted error is manufacturer specified accuracy in dbar.                                                                                                                                    The quoted error is manufacturer specified accuracy with respect to ITS-90 at time of laboratory calibration.                                                                                                                                                   Significant salinity drift detected. OWC least squares fit adopted. Salinity also adjusted for effects of pressure adjustment. The quoted error is max[0.01, 1xOWC uncertainty] in PSS-78.                                                                      202202041144142022020411441420220204114414  AO  ARCAADJP                                                                    20200619170903    IP                G�O�G�O�G�O�                AO  ARGQQCPL                                                                    20200619170903  QCP$                G�O�G�O�G�O�5F03E           AO  ARGQQCPL                                                                    20200619170903  QCF$                G�O�G�O�G�O�0               UW  ARSQUWQC    WOD & nearby Argo as visual check                               20220204114414  IP                  G�O�G�O�G�O�                