CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             
   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       2020-06-19T17:09:23Z creation      
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
_FillValue                    ��Argo profile    3.1 1.2 19500101000000  20200619170923  20220204114423  5905979 UW, Argo                                                        STEPHEN RISER                                                   PRES            TEMP            PSAL               ~A   AO  7662                            2C  D   APEX                            8312                            080318                          846 @����1   @������l@7 ě��T�b�p��
=1   GPS     Primary sampling: mixed [deeper than nominal 985dbar: discrete; nominal 985dbar to surface: 2dbar-bin averaged]                                                                                                                                                    ~A   B   B   @�  @�  A   A   A@  A`  A�  A�  A�  A�  A�  A�  A�  A�  B   B  B  BffB   B(  B0  B8  B@  BH  BP  BX  B`  Bh  Bp  Bx  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�33B�  B�  B�  B�  B�  B�  C   C  C  C  C  C
  C  C  C  C  C  C  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D1��D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=�fD>  D>� D?  D?�fD@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DPy�DQ  DQ� DR  DR� DS  DS� DT  DT� DUfDU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Dr��Ds� Dt  Dt� DtٚDy��D�(�D�UD��
D��D��D�Y�D���D��fD��D�T{D�� D��\D��D�L�Dڑ�D��3D��D�g
D��D���111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  @�33@�33@�33A��A=��A]��A}��A���A���A���A���A���A���A���A���BffBffB��BffB'ffB/ffB7ffB?ffBGffBOffBWffB_ffBgffBoffBwffBffB��3B��3B��3B��3B��3B��3B��3B��3B��3B��3B��3B��3B��3B��3B��3B��3Bó3Bǳ3B˳3Bϳ3Bӳ3B׳3B۳3B߳3B��fB�3B�3B�3B�3B��3B��3B��3CٚCٚCٚCٚC	ٚCٚCٚCٚCٚCٚCٚCٚCٚCٚCٚCٚC!ٚC#ٚC%ٚC'ٚC)ٚC+ٚC-ٚC/ٚC1ٚC3ٚC5ٚC7ٚC9ٚC;ٚC=ٚC?ٚCAٚCCٚCEٚCGٚCIٚCKٚCMٚCOٚCQٚCSٚCUٚCWٚCYٚC[ٚC]ٚC_ٚCaٚCcٚCeٚCgٚCiٚCkٚCmٚCoٚCqٚCsٚCuٚCwٚCyٚC{ٚC}ٚCٚC���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���D vfD �fDvfD�fDvfD�fDvfD�fDvfD�fDvfD�fDvfD�fDvfD�fDvfD�fD	vfD	�fD
vfD
�fDvfD�fDvfD�fDvfD�fDvfD�fDvfD�fDvfD�fDvfD�fDvfD�fDvfD�fDvfD�fDvfD�fDvfD�fDvfD�fDvfD�fDvfD�fDvfD�fDvfD�fDvfD�fDvfD�fDvfD�fDvfD�fD vfD �fD!vfD!�fD"vfD"�fD#vfD#�fD$vfD$�fD%vfD%�fD&vfD&�fD'vfD'�fD(vfD(�fD)vfD)�fD*vfD*�fD+vfD+�fD,vfD,�fD-vfD-�fD.vfD.�fD/vfD/�fD0vfD0�fD1vfD1� D2vfD2�fD3vfD3�fD4vfD4�fD5vfD5�fD6vfD6�fD7vfD7�fD8vfD8�fD9vfD9�fD:vfD:�fD;vfD;�fD<vfD<�fD=|�D=�fD>vfD>�fD?|�D?�fD@vfD@�fDAvfDA�fDBvfDB�fDCvfDC�fDDvfDD�fDEvfDE�fDFvfDF�fDGvfDG�fDHvfDH�fDIvfDI�fDJvfDJ�fDKvfDK�fDLvfDL�fDMvfDM�fDNvfDN�fDOvfDO�fDPp DP�fDQvfDQ�fDRvfDR�fDSvfDS�fDTvfDT��DUvfDU�fDVvfDV�fDWvfDW�fDXvfDX�fDYvfDY�fDZvfDZ�fD[vfD[�fD\vfD\�fD]vfD]�fD^vfD^�fD_vfD_�fD`vfD`�fDavfDa�fDbvfDb�fDcvfDc�fDdvfDd�fDevfDe�fDfvfDf�fDgvfDg�fDhvfDh�fDivfDi�fDjvfDj�fDkvfDk�fDlvfDl�fDmvfDm�fDnvfDn�fDovfDo�fDpvfDp�fDqvfDq�fDrvfDr� DsvfDs�fDtvfDt� Dy��D�$)D�PRD��=D��RD���D�T�D��D��D� D�O�D��3D�ڏD��D�H Dڌ�D��fD� D�b=D��D���111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  @��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��A�7LA�A�A�O�A�K�A�G�A�Q�A�Q�A�Q�A�S�A�VA�\)A�ZA�XA�XA�ZA�\)A�\)A�ZA�ZA�^5A�`BA�`BA�bNA�hsA�jA�jA�n�A�n�A�p�A�n�A�n�A�p�A�p�A�p�A�p�A�r�A�t�A�t�A�t�A�p�A�n�A�n�A�jA�n�A�n�A�l�A�n�A�l�A�n�A�p�A�p�A�jA�bNA�Q�A�E�A�ffA�ȴA�C�A�ȴA��A�;dA�XA��A��A��HA�;dA�E�A��#A�n�A�bA�ĜA�n�A��`A�G�A�~�A�r�A�dZA�\)A�XA���A�n�A�A���A�A�A�x�A�O�A�dZA�A�?}A�VA��TA�A���A�M�A�A�z�A� �A�5?A�
=A� �A���A��A��yA��-A�n�A���A��!A�XA���A��/A��DA���A�A��A~=qA}O�A{�AzbAv�jAt�jAs�^Aq��Ao�wAn$�AlI�AjȴAioAg�hAd��AbĜA_��A]VA[�
AZ�DAW�#AVQ�AT^5AR��AQ�AQ7LAO��AM��AJQ�AG33AE�ADVAB^5AA�A@r�A@bA?�TA>��A<��A:�`A9�
A9p�A9"�A8�A8(�A7&�A6{A4�+A3t�A2z�A1hsA1?}A0�DA/��A.�DA-A,VA+|�A*�!A)�A'x�A%�mA$��A#�-A"��A"A�A!t�A   A/A�+AƨA��A��A��AO�A33A\)A��A��A�DA��AK�A~�A(�A�TAz�A33A�`A�+A��A��An�A-A�hAp�Ap�A�A&�A��Az�A�A��A9XA�A�A
  A	�A�A	�-A	p�A	��A	�wA
�A	VA�/A��A��A�hA�yA1A7LA r�@�M�@���@��P@�
=@��\@�dZ@�V@�bN@��9@�^5@�{@�  @���@�(�@��@�h@�~�@�D@�`B@�x�@۝�@��y@�ff@�z�@�9X@�O�@�{@ёh@�  @���@��@�I�@�K�@�33@�^5@Ȭ@�=q@��@�bN@Õ�@��@�ff@���@�S�@�^5@�&�@�"�@���@�@���@���@��^@��@��u@�I�@��F@�dZ@�
=@��!@��\@�-@��h@�7L@�1@��@�K�@���@��@�  @���@���@�E�@�@���@�&�@�&�@�hs@��@���@�Z@�  @�"�@�v�@�n�@���@�&�@��h@��7@�%@���@�V@���@��D@�9X@�1'@�b@�|�@�
=@��R@���@�n�@��@�x�@�j@�@���@��@��@��h@�=q@��+@�=q@�$�@�X@�p�@���@��D@��u@���@��u@��@���@�5?@�X@��`@���@�z�@���@�V@��@�&�@�Ĝ@���@�+@�ȴ@�ȴ@��y@��H@�+@�@�
=@��\@�ff@�=q@�{@��T@��-@��h@�`B@�/@�%@���@��j@��D@�j@�Z@�Z@�Q�@�I�@�I�@��@�j@��!@�9X@�Q�@���@��@�dZ@�;d@��@��@���@���@�%@�V@�/@�X@�X@�7L@��`@��@��u@���@��@�z�@��D@���@�\)@�
=@�@���@��\@�v�@�V@�E�@�5?@�5?@�5?@��@���@�/@���@�bN@���@���@��w@���@��@��
@���@�\)@��!@��+@�V@�E�@��+@�~�@�^5@��@��@���@�`B@�O�@��@��j@���@�I�@��F@��P@�\)@�33@�
=@��y@���@��\@��+@�n�@�V@�5?@��T@��7@�%@�Ĝ@�I�@�  @��
@��w@��P@�\)@�
=@��H@��@���@��@z�8@r�"@l��@e�@\�@SX�@M��@G��@B�@94@2��@/�@*�}@% \@ D�@�8@V@�\@�T@W?111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111  A�7LA�A�A�O�A�K�A�G�A�Q�A�Q�A�Q�A�S�A�VA�\)A�ZA�XA�XA�ZA�\)A�\)A�ZA�ZA�^5A�`BA�`BA�bNA�hsA�jA�jA�n�A�n�A�p�A�n�A�n�A�p�A�p�A�p�A�p�A�r�A�t�A�t�A�t�A�p�A�n�A�n�A�jA�n�A�n�A�l�A�n�A�l�A�n�A�p�A�p�A�jA�bNA�Q�A�E�A�ffA�ȴA�C�A�ȴA��A�;dA�XA��A��A��HA�;dA�E�A��#A�n�A�bA�ĜA�n�A��`A�G�A�~�A�r�A�dZA�\)A�XA���A�n�A�A���A�A�A�x�A�O�A�dZA�A�?}A�VA��TA�A���A�M�A�A�z�A� �A�5?A�
=A� �A���A��A��yA��-A�n�A���A��!A�XA���A��/A��DA���A�A��A~=qA}O�A{�AzbAv�jAt�jAs�^Aq��Ao�wAn$�AlI�AjȴAioAg�hAd��AbĜA_��A]VA[�
AZ�DAW�#AVQ�AT^5AR��AQ�AQ7LAO��AM��AJQ�AG33AE�ADVAB^5AA�A@r�A@bA?�TA>��A<��A:�`A9�
A9p�A9"�A8�A8(�A7&�A6{A4�+A3t�A2z�A1hsA1?}A0�DA/��A.�DA-A,VA+|�A*�!A)�A'x�A%�mA$��A#�-A"��A"A�A!t�A   A/A�+AƨA��A��A��AO�A33A\)A��A��A�DA��AK�A~�A(�A�TAz�A33A�`A�+A��A��An�A-A�hAp�Ap�A�A&�A��Az�A�A��A9XA�A�A
  A	�A�A	�-A	p�A	��A	�wA
�A	VA�/A��A��A�hA�yA1A7LA r�@�M�@���@��P@�
=@��\@�dZ@�V@�bN@��9@�^5@�{@�  @���@�(�@��@�h@�~�@�D@�`B@�x�@۝�@��y@�ff@�z�@�9X@�O�@�{@ёh@�  @���@��@�I�@�K�@�33@�^5@Ȭ@�=q@��@�bN@Õ�@��@�ff@���@�S�@�^5@�&�@�"�@���@�@���@���@��^@��@��u@�I�@��F@�dZ@�
=@��!@��\@�-@��h@�7L@�1@��@�K�@���@��@�  @���@���@�E�@�@���@�&�@�&�@�hs@��@���@�Z@�  @�"�@�v�@�n�@���@�&�@��h@��7@�%@���@�V@���@��D@�9X@�1'@�b@�|�@�
=@��R@���@�n�@��@�x�@�j@�@���@��@��@��h@�=q@��+@�=q@�$�@�X@�p�@���@��D@��u@���@��u@��@���@�5?@�X@��`@���@�z�@���@�V@��@�&�@�Ĝ@���@�+@�ȴ@�ȴ@��y@��H@�+@�@�
=@��\@�ff@�=q@�{@��T@��-@��h@�`B@�/@�%@���@��j@��D@�j@�Z@�Z@�Q�@�I�@�I�@��@�j@��!@�9X@�Q�@���@��@�dZ@�;d@��@��@���@���@�%@�V@�/@�X@�X@�7L@��`@��@��u@���@��@�z�@��D@���@�\)@�
=@�@���@��\@�v�@�V@�E�@�5?@�5?@�5?@��@���@�/@���@�bN@���@���@��w@���@��@��
@���@�\)@��!@��+@�V@�E�@��+@�~�@�^5@��@��@���@�`B@�O�@��@��j@���@�I�@��F@��P@�\)@�33@�
=@��y@���@��\@��+@�n�@�V@�5?@��T@��7@�%@�Ĝ@�I�@�  @��
@��w@��P@�\)@�
=@��H@��G�O�@��@z�8@r�"@l��@e�@\�@SX�@M��@G��@B�@94@2��@/�@*�}@% \@ D�@�8@V@�\@�T@W?111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111  ;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oG�O�;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oB
ŢB
ĜB
ĜB
ĜB
ĜB
ĜB
ĜB
ĜB
ĜB
ĜB
ĜB
ĜB
ĜB
ĜB
ĜB
ĜB
ĜB
ĜB
ĜB
ĜB
ĜB
ĜB
ĜB
ŢB
ŢB
ŢB
ŢB
ŢB
ŢB
ŢB
ŢB
ŢB
ŢB
ŢB
ƨB
ƨB
ŢB
ŢB
ŢB
ŢB
ĜB
ĜB
ŢB
ĜB
ŢB
ƨB
ƨB
ƨB
��B
��B
��B
�B
�TB
�B
��B?}B��B�TB�9B�+B��B�B�yB��BBB$�B/B�B�B�B{BbB�B �B �B �B"�B(�B"�B�B��B�B�;B�B�oB�hB��B�B��B�bB�B�BcTB33BDB�B(�B)�B�BVB\BB
�B
ɺB
��B
�uB
z�B
r�B
ffB
P�B
D�B
(�B
oB	��B	�B	�B	�B	�B	ŢB	�qB	�9B	��B	��B	�DB	�B	u�B	hsB	ZB	N�B	8RB	-B	!�B	�B	bB	B��B�B�B�B�fB�BB�B��B�}B�?B��B��B�B�9B�LB�XB�B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B�bB�PB�DB�+B�By�Br�Bo�Bl�Bk�BhsBffBcTBbNBbNB`BB]/B\)BZBiyBt�Be`BR�BM�BO�BM�BK�BL�BR�BW
Bk�B�B��B��B��B�hB�DB�{B��B��B��B��B��B��B��B��B�!B�B�9B�B�{B��B�VB��B�!B�jBǮB�B��B�BB�B�jB�wB�FB�B��B��B��B��B��B��B�?B�B�B��B��B��B�qB�FB�!B��B��B�uB�JB�Bx�Bu�Bt�Bx�Bu�Br�Bm�BhsBk�Bk�BhsBffBgmBffBffBe`BcTBe`Be`BffBiyBk�Bl�Bp�Br�Bs�Bu�Bt�Bx�Bx�By�Bz�B}�B�B�%B�1B�PB�VB�VB�\B�oB�{B��B��B��B��B��B��B��B��B�B�B�!B�'B�-B�?B�LB�^B�jB�wB�wB�}B��BŢBȴB��B��B��B�
B�B�B�
B�B�BB�NB�TB�ZB�fB�sB�sB�B�B�B�B�B�B�B�B��B��B	B	DB	DB	JB	VB	{B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	%�B	,B	-B	1'B	1'B	2-B	1'B	1'B	49B	8RB	8RB	?}B	B�B	C�B	E�B	F�B	J�B	M�B	N�B	O�B	Q�B	VB	ZB	[#B	]/B	`BB	bNB	cTB	dZB	e`B	ffB	gmB	iyB	n�B	q�B	r�B	n�B	r�B	r�B	s�B	t�B	v�B	x�B	y�B	z�B	� B	�B	�+B	�7B	�DB	�DB	�DB	�\B	�bB	�oB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	��B	��B	�B	�B	�B	�'B	�9B	�RB	�^B	�dB	�dB	�dB	�qB	�wB	B	ÖB	ÖB	ÖB	B	ÖB	ĜB	ŢB	ƨB	ǮB	ɺB	ɺB	ɺB	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�#B	�#B	�#B	�)B	�/B	�/B	�5B	�;B	�BB	�HB	�NB	�NB	�NB	��B	�}B
	RB
�B
(>B
+�B
9�B
A�B
F�B
J�B
IB
P�B
VmB
]dB
^�B
c�B
gB
kQB
q'B
w�B
{�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111  B
�<B
�6B
�6B
�6B
�6B
�6B
�6B
�6B
�6B
�6B
�6B
�6B
�6B
�6B
�6B
�6B
�6B
�6B
�6B
�6B
�6B
�6B
�6B
�<B
�<B
�<B
�<B
�<B
�<B
�<B
�<B
�<B
�<B
�<B
�BB
�BB
�<B
�<B
�<B
�<B
�6B
�6B
�<B
�6B
�<B
�BB
�BB
�BB
�rB
ŅB
ƋB
˩B
��B
�FB
�kB3B�-B��B��Bz�B�YB˓B��B�\B��B��B[B"�B>B&BB�B�BBFBFBFBRBvBRB:B�aB�B��B��B� B��B�B��B�=B��Bv�Bx�BV�B&�B
��B
&B�B�BKB�B�B
��B
�/B
�bB
��B
�#B
n�B
fbB
ZB
D�B
8SB
�B
+B	�B	�dB	�jB	�LB	��B	�gB	�7B	� B	��B	�jB	B	t�B	i�B	\CB	M�B	B�B	,(B	 �B	�B	B	=B��B��B�B�}B�^B�FB�#B��B��B�bB�%B��B��B��B� B�3B�?B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B�tB�PB�>B2B{BwBm�Bf�Bc�B`}B_xB\fBZYBWHBVBBVBBT7BQ$BPBNB]mBh�BYUBF�BA�BC�BA�B?�B@�BF�BKB_zBt�B��B��B��B�ZB7B�mB�yB��B��B��B��B��B��B��B�B��B�)B��B�nB�tB�JB��B�B�ZB��B�B��B�B�B��B�[B�hB�8B�B��B��B��B��B��B��B�2B�B��B��B��B��B�dB�9B�B��B��B�mB�CBvBl�Bi�Bh�Bl�Bi�Bf�Ba�B\qB_�B_�B\qBZdB[kBZeBZeBY_BWSBY_BY_BZeB]xB_�B`�Bd�Bf�Bg�Bi�Bh�Bl�Bl�Bm�Bn�Bq�BwBz#B|.B�MB�SB�SB�YB�lB�wB��B��B��B��B��B��B��B��B��B�B�B�!B�'B�9B�FB�WB�cB�pB�pB�vB�|B��B��B��B��B��B�B�B�B�B�B�9B�EB�KB�QB�\B�iB�iB�uB�{B��B�B�B�B�B�B��B��B� B�7B�8B	 =B	IB	nB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	 �B	%B	%B	&B	%B	%B	(*B	,BB	,BB	3lB	6~B	7�B	9�B	:�B	>�B	A�B	B�B	C�B	E�B	I�B	N
B	OB	QB	T/B	V:B	W@B	XFB	YLB	ZRB	[YB	]eB	b�B	e�B	f�B	b�B	f�B	f�B	g�B	h�B	j�B	l�B	m�B	n�B	s�B	y	B	{B	}!B	-B	-B	-B	�EB	�KB	�XB	�jB	�jB	�jB	�uB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�8B	�DB	�JB	�JB	�JB	�WB	�\B	�tB	�{B	�{B	�{B	�tB	�{B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	½B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�B	�B	�B	�B	�%B	�+B	�1B	�1G�O�B	�B	�^B	�2B
�B
B
cB
-iB
5�B
:�B
>iB
<�B
D�B
JIB
Q@B
R�B
W�B
Z�B
_-B
eB
k�B
os111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111  <#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
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
PRES            TEMP            PSAL            PRES_ADJUSTED = PRES - dP                                                                                                                                                                                                                                       none                                                                                                                                                                                                                                                            PSAL_ADJUSTED = PSAL + dS, where dS is calculated from a potential conductivity (ref to 0 dbar) multiplicative adjustment factor r.                                                                                                                             dP =0.15 dbar.                                                                                                                                                                                                                                                  none                                                                                                                                                                                                                                                            r =0.9997(+/-0.0001), vertically averaged dS =-0.012(+/-0.004) in PSS-78.                                                                                                                                                                                       Pressures adjusted by using pressure offset at the sea surface. The quoted error is manufacturer specified accuracy in dbar.                                                                                                                                    The quoted error is manufacturer specified accuracy with respect to ITS-90 at time of laboratory calibration.                                                                                                                                                   Significant salinity drift detected. OWC least squares fit adopted. Salinity also adjusted for effects of pressure adjustment. The quoted error is max[0.01, 1xOWC uncertainty] in PSS-78.                                                                      202202041144232022020411442320220204114423  AO  ARCAADJP                                                                    20200619170923    IP                G�O�G�O�G�O�                AO  ARGQQCPL                                                                    20200619170923  QCP$                G�O�G�O�G�O�5F03E           AO  ARGQQCPL                                                                    20200619170923  QCF$                G�O�G�O�G�O�0               UW  ARSQUWQC    WOD & nearby Argo as visual check                               20220204114423  IP                  G�O�G�O�G�O�                