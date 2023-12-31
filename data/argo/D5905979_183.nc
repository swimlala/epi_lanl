CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             
   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       2020-10-28T01:02:07Z creation      
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
_FillValue                    ��Argo profile    3.1 1.2 19500101000000  20201028010207  20220204114429  5905979 UW, Argo                                                        STEPHEN RISER                                                   PRES            TEMP            PSAL               �A   AO  7662                            2C  D   APEX                            8312                            080318                          846 @�B�#vn1   @�Bݲ@�,@5��hr��b�bM��1   GPS     Primary sampling: mixed [deeper than nominal 985dbar: discrete; nominal 985dbar to surface: 2dbar-bin averaged]                                                                                                                                                    �A   B   B   @���@�  A   A   AA��A`  A�  A�  A�  A�  A�  A�  A�  A�  B   B  B  B  B   B(  B0  B8  B@  BH  BP  BX  B`  Bh  BpffBx  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  C   C  C  C  C  C
  C  C  C  C  C  C  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV�CX  CZ  C\  C^  C_�fCb  Cd  Cf  Ch  Cj  Cl  Cm�fCp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D��D� D  D� D  D� D��Dy�D��D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D%��D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DHy�DH��DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]y�D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� DnfDn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dty�Dt��Dy�)D���D�YHD��3D�ӅD�
D�YHD���D��3D��D�_�D���D�� D��D�QHDڍqD��3D��D�XRD��D��111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  @���@�33@�33A��A?34A]��A}��A���A���A���A���A���A���A���A���BffBffBffBffB'ffB/ffB7ffB?ffBGffBOffBWffB_ffBgffBo��BwffBffB��3B��3B��3B��3B��3B��3B��3B��3B��3B��3B��3B��3B��3B��3B��3B��3Bó3Bǳ3B˳3Bϳ3Bӳ3B׳3B۳3B߳3B�3B�3B�3B�3B�3B��3B��3B��3CٚCٚCٚCٚC	ٚCٚCٚCٚCٚCٚCٚCٚCٚCٚCٚCٚC!ٚC#ٚC%ٚC'ٚC)ٚC+ٚC-ٚC/ٚC1ٚC3ٚC5ٚC7ٚC9ٚC;ٚC=ٚC?ٚCAٚCCٚCEٚCGٚCIٚCKٚCMٚCOٚCQٚCSٚCU�4CWٚCYٚC[ٚC]ٚC_� CaٚCcٚCeٚCgٚCiٚCkٚCm� CoٚCqٚCsٚCuٚCwٚCyٚC{ٚC}ٚCٚC���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���D vfD �fDvfD�fDvfD�fDvfD�fDvfD�fDvfD�fDvfD�fDvfD�fDvfD�fD	vfD	�fD
vfD
�fDvfD� DvfD�fDvfD�fDvfD� Dp D� DvfD�fDvfD�fDvfD�fDvfD�fDvfD�fDvfD�fDvfD�fDvfD�fDvfD�fDvfD�fDvfD�fDvfD�fDvfD�fDvfD�fDvfD�fDvfD�fD vfD �fD!vfD!�fD"vfD"�fD#vfD#�fD$vfD$�fD%vfD%� D&vfD&�fD'vfD'�fD(vfD(�fD)vfD)�fD*vfD*�fD+vfD+�fD,vfD,�fD-vfD-�fD.vfD.�fD/vfD/�fD0vfD0�fD1vfD1�fD2vfD2�fD3vfD3�fD4vfD4�fD5vfD5�fD6vfD6�fD7vfD7�fD8vfD8�fD9vfD9�fD:vfD:�fD;vfD;�fD<vfD<�fD=vfD=�fD>vfD>�fD?vfD?�fD@vfD@�fDAvfDA�fDBvfDB�fDCvfDC�fDDvfDD�fDEvfDE�fDFvfDF�fDGvfDG�fDHp DH� DIvfDI�fDJvfDJ�fDKvfDK�fDLvfDL�fDMvfDM�fDNvfDN�fDOvfDO�fDPvfDP�fDQvfDQ�fDRvfDR�fDSvfDS�fDTvfDT�fDUvfDU�fDVvfDV�fDWvfDW�fDXvfDX�fDYvfDY�fDZvfDZ�fD[vfD[�fD\vfD\�fD]p D]�fD^vfD^�fD_vfD_�fD`vfD`�fDavfDa�fDbvfDb�fDcvfDc�fDdvfDd�fDevfDe�fDfvfDf�fDgvfDg�fDhvfDh�fDivfDi�fDjvfDj�fDkvfDk�fDlvfDl�fDmvfDm��DnvfDn�fDovfDo�fDpvfDp�fDqvfDq�fDrvfDr�fDsvfDs�fDtp Dt�3Dy��D��D�T{D��fD�θD�=D�T{D���D��fD��D�Z�D���D��3D��D�L{Dڈ�D��fD��D�S�D��D��G111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  @��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��A�A�A�A���A�ĜA���A�ĜA���A���A���A���A��
A��#A���A��#A��#A���A��HA��;A���AӲ-Aә�Aӏ\A�jA�E�A�%AҾwAЃA���Aʰ!AƼjA�ZA�p�A��A��A�G�A�K�A�Q�A�;dA�bNA�|�A�+A���A���A�S�A�7LA�oA� �A�ĜA� �A�ƨA��RA�M�A�t�A��A�(�A���A���A�+A��\A��A��A�S�A�
=A�`BA�{A��FA�l�A���A�A��\A�G�A���A�%A�1'A���A��A�A���A�z�A��DA���A�~�A��A�oA���A���A���A���A�{A�Q�A���A�ffA���A�ƨA��HA��A���A�x�A��
A��A�I�A�ĜA�M�A���A���A���A��\A��A�$�A}��AzM�AuC�Ao��Ak��Aj�\Af5?Ac��Aa��A^��A]\)A[l�AYVAW�wAV��AUK�AR�!AQ�AP��ANI�AK�
AI\)AH-AFZAE?}ACXAAG�A?�A>�A=/A=%A<~�A;��A;?}A:bA8v�A7l�A7XA7&�A6�9A5hsA4ffA2��A1�A0��A0v�A0{A/7LA.�A-A+x�A*n�A*-A)�
A(��A(A'XA&�jA&$�A%��A$��A$A�A"��A!�mA �RA��AG�A�uAZA�mA"�A(�AAZAƨAl�A�AQ�A{A��Av�A��A�A��A5?A��A�PA��A��A7LA^5A
��A	p�A��A��A��AȴA=qA33A�/A�RAffA  A ��A $�@�o@�5?@�hs@�z�@�1@�l�@���@��@�  @�dZ@��@�@�b@�@�h@�P@��
@�\@�9@�n�@�p�@�7L@�bN@�hs@�+@�ȴ@���@�/@�Ĝ@�b@�@�{@ٲ-@؋D@�+@�x�@�1'@�~�@�I�@���@Χ�@̼j@���@ɺ^@���@���@�9X@�I�@��@�33@Ų-@�V@��@ÍP@Å@�|�@�
=@��@�%@��
@�S�@�
=@���@�@���@�  @���@���@�p�@�$�@�X@�  @�1@���@�\)@���@�p�@�  @���@�dZ@��\@�J@��h@�?}@���@��j@�|�@��@�=q@��h@�Z@�Z@�dZ@��@��\@�n�@���@��u@��@��P@�S�@���@�n�@���@�hs@��@��u@��@�Z@� �@�|�@�C�@�t�@��P@�t�@�"�@��y@�v�@��#@���@�`B@�?}@�%@���@���@�A�@���@���@�\)@���@���@�V@��@��@��@���@�hs@�O�@�/@���@��`@���@��D@�A�@�b@� �@�1@��w@�|�@���@�n�@���@��-@���@��7@�p�@�G�@��@��`@�z�@�(�@���@�1'@��@��;@���@���@�o@�ȴ@���@�~�@�n�@�E�@��T@���@�p�@�X@�7L@��@���@�`B@���@��#@�O�@��@�%@�/@��@���@��j@�1'@�r�@�Q�@�b@�33@�ƨ@���@���@���@��
@���@��!@��@�hs@�V@���@��@���@���@�~�@���@��H@�
=@�@�S�@�\)@��H@�@��@��+@�?}@�Ĝ@��9@�z�@�Z@�9X@��@��w@�\)@���@���@���@���@�o@�|�@���@�|�@���@��@�r�@�bN@���@�\)@���@�o@��@��H@���@�M�@���@��h@�?}@��@�V@��@���@�bN@�1'@���@���@��F@�dZ@���@�ff@�{@���@��^@���@�p�@�X@�?}@�7L@�&�@�&�@��@{s@s�@j�@a�D@Z.�@Q�@Jں@C�;@=a�@8|�@1ϫ@-5�@(��@#�0@ c�@��@7@/�@dZ@33111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111  A�A�A�A���A�ĜA���A�ĜA���A���A���A���A��
A��#A���A��#A��#A���A��HA��;A���AӲ-Aә�Aӏ\A�jA�E�A�%AҾwAЃA���Aʰ!AƼjA�ZA�p�A��A��A�G�A�K�A�Q�A�;dA�bNA�|�A�+A���A���A�S�A�7LA�oA� �A�ĜA� �A�ƨA��RA�M�A�t�A��A�(�A���A���A�+A��\A��A��A�S�A�
=A�`BA�{A��FA�l�A���A�A��\A�G�A���A�%A�1'A���A��A�A���A�z�A��DA���A�~�A��A�oA���A���A���A���A�{A�Q�A���A�ffA���A�ƨA��HA��A���A�x�A��
A��A�I�A�ĜA�M�A���A���A���A��\A��A�$�A}��AzM�AuC�Ao��Ak��Aj�\Af5?Ac��Aa��A^��A]\)A[l�AYVAW�wAV��AUK�AR�!AQ�AP��ANI�AK�
AI\)AH-AFZAE?}ACXAAG�A?�A>�A=/A=%A<~�A;��A;?}A:bA8v�A7l�A7XA7&�A6�9A5hsA4ffA2��A1�A0��A0v�A0{A/7LA.�A-A+x�A*n�A*-A)�
A(��A(A'XA&�jA&$�A%��A$��A$A�A"��A!�mA �RA��AG�A�uAZA�mA"�A(�AAZAƨAl�A�AQ�A{A��Av�A��A�A��A5?A��A�PA��A��A7LA^5A
��A	p�A��A��A��AȴA=qA33A�/A�RAffA  A ��A $�@�o@�5?@�hs@�z�@�1@�l�@���@��@�  @�dZ@��@�@�b@�@�h@�P@��
@�\@�9@�n�@�p�@�7L@�bN@�hs@�+@�ȴ@���@�/@�Ĝ@�b@�@�{@ٲ-@؋D@�+@�x�@�1'@�~�@�I�@���@Χ�@̼j@���@ɺ^@���@���@�9X@�I�@��@�33@Ų-@�V@��@ÍP@Å@�|�@�
=@��@�%@��
@�S�@�
=@���@�@���@�  @���@���@�p�@�$�@�X@�  @�1@���@�\)@���@�p�@�  @���@�dZ@��\@�J@��h@�?}@���@��j@�|�@��@�=q@��h@�Z@�Z@�dZ@��@��\@�n�@���@��u@��@��P@�S�@���@�n�@���@�hs@��@��u@��@�Z@� �@�|�@�C�@�t�@��P@�t�@�"�@��y@�v�@��#@���@�`B@�?}@�%@���@���@�A�@���@���@�\)@���@���@�V@��@��@��@���@�hs@�O�@�/@���@��`@���@��D@�A�@�b@� �@�1@��w@�|�@���@�n�@���@��-@���@��7@�p�@�G�@��@��`@�z�@�(�@���@�1'@��@��;@���@���@�o@�ȴ@���@�~�@�n�@�E�@��T@���@�p�@�X@�7L@��@���@�`B@���@��#@�O�@��@�%@�/@��@���@��j@�1'@�r�@�Q�@�b@�33@�ƨ@���@���@���@��
@���@��!@��@�hs@�V@���@��@���@���@�~�@���@��H@�
=@�@�S�@�\)@��H@�@��@��+@�?}@�Ĝ@��9@�z�@�Z@�9X@��@��w@�\)@���@���@���@���@�o@�|�@���@�|�@���@��@�r�@�bN@���@�\)@���@�o@��@��H@���@�M�@���@��h@�?}@��@�V@��@���@�bN@�1'@���@���@��F@�dZ@���@�ff@�{@���@��^@���@�p�@�X@�?}@�7L@�&�G�O�@��@{s@s�@j�@a�D@Z.�@Q�@Jں@C�;@=a�@8|�@1ϫ@-5�@(��@#�0@ c�@��@7@/�@dZ@33111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111  ;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oG�O�;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oB7LB8RB7LB7LB7LB7LB7LB7LB7LB8RB8RB9XB:^B9XB:^B:^B9XB?}BB�BD�BG�BF�BH�BJ�BK�BH�B@�B)�B/B;dBD�B(�B#�B49B6FBJ�B[#B^5BhsBu�B}�B{�Bv�By�B��B�B�9B��B�B�TB��B%B	7B\B�B'�B+B2-B>wBB�BH�BS�BO�BVBZB[#B\)B\)BZB]/BW
BR�BO�BG�B@�B9XB,B!�B#�B�BhBB��B�B�ZB�5B��BǮB�FB�B��B��B�oB�1Bv�BhsB<jB$�BB
�B
�TB
�
B
B
��B
��B
�bB
}�B
k�B
Q�B
C�B
/B
bB	�B	ȴB	��B	��B	�+B	k�B	aHB	N�B	B�B	8RB	)�B	!�B	�B	hB	B��B��B�ZB�BɺBB�^B�9B�B��B��B��B��B�{B�oB�VB�VB�JB�=B�B�B�B�B�B�B~�B{�Bx�Bw�Bx�Bx�Bu�Bv�Br�Bp�Bp�Bo�Bm�BjBk�B5?BgmBhsBhsBiyBjBhsBffBdZBe`BffBgmBjBl�Bm�Bo�Bp�Bp�Bq�Bq�Bq�Bq�Bq�Br�Bq�Bp�Bo�Bm�Bk�BjBjBhsBhsBhsBcTBcTBe`B`BB^5B[#B[#B]/B`BBbNBdZBdZBaHB]/B^5BdZBp�Bp�Br�Bq�Br�Bs�Br�Bt�Bv�B}�B�%B�B� B{�Bv�Bt�Bv�Bu�Bw�Bv�Bx�Bv�Bz�B~�B}�Bz�By�B{�B}�B�B�B� B~�B|�B|�B|�Bx�Bw�Bv�Bp�Bo�Br�Bu�B{�B�B�=B�PB�bB�bB�bB�oB�oB�oB�hB�hB�bB�\B�hB�{B��B��B��B��B��B��B��B��B�B�B��B�!B�'B�'B�'B�-B�9B�RB�XB�qB�qB�qB�wB��B��BŢBȴB��B��B��B��B��B�
B�B�B�)B�HB�NB�TB�ZB�mB�yB�B�B��B��B��B��B	B	B	1B	
=B	DB	PB	\B	oB	�B	�B	�B	#�B	%�B	'�B	(�B	+B	-B	/B	0!B	2-B	33B	49B	49B	7LB	9XB	:^B	=qB	B�B	E�B	G�B	I�B	J�B	K�B	M�B	N�B	P�B	R�B	W
B	ZB	[#B	^5B	bNB	cTB	e`B	gmB	hsB	jB	l�B	n�B	q�B	v�B	z�B	|�B	�B	�B	�B	�B	�B	�+B	�1B	�1B	�1B	�=B	�JB	�\B	�bB	�uB	��B	��B	��B	��B	��B	��B	�B	�B	�B	�!B	�9B	�3B	�9B	�jB	�^B	�jB	�jB	�qB	�qB	ÖB	ŢB	ǮB	ȴB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�
B	�B	�#B	�)B	�#B	�)B	�)B	�#B	�B	�
B	�
B	�B	�
B	�B	�B	�B	�B	�
B	�
B	�
B	�B	�B	�/B	�;B	�BB	�`B	�yB	�yB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B
	7B
yB
�B
+6B
3B
<�B
B�B
G�B
N"B
SB
Y�B
^B
b�B
g�B
j�B
o B
raB
w2B
z^B
~�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111  B'B(B'B'B'B'B'B'B'B(B(B)B*%B)B*%B*%B)B/DB2UB4bB7tB6nB8zB:�B;�B8zB0JB�B�B+.B4fB�B�B$B&B:�BJ�BM�BX;Be�Bm�Bk�Bf�Bi�B�PB��B��B�HB��B�B�B��B��B�B>B�B�B!�B.*B2BB8fBC�B?�BE�BI�BJ�BK�BK�BI�BL�BF�BB�B?�B7cB09B)B�B�B�ByB%B��B�B�HB�B��B¹B�vB�B��B��B�oB�?BxBf�BXIB,FB�B
��B
�B
�=B
��B
�}B
��B
��B
�WB
m�B
[B
A�B
3�B
B
 iB	�B	��B	�B	��B	wGB	[�B	QiB	>�B	2�B	(yB	%B	�B	�B	�B�6B�B��BԎB�SB��B��B��B�uB�QB�!B��B��B��B��B��B~�B~�B|�Bz�BucBt]BsWBrQBqKBrQBo@Bl.BiBhBiBiBfBgBb�B`�B`�B_�B]�BZ�B[�G�O�BW�BX�BX�BY�BZ�BX�BV�BT�BU�BV�BW�BZ�B\�B]�B_�B`�B`�Ba�Ba�Ba�Ba�Ba�Bb�Ba�B`�B_�B]�B[�BZ�BZ�BX�BX�BX�BS�BS�BU�BP�BN�BKxBKxBM�BP�BR�BT�BT�BQ�BM�BN�BT�B`�B`�BcBa�BcBd
BcBeBgBnGBvwBqYBpSBl;BgBeBgBfBh$BgBi*BgBk7BoOBnJBk7Bj1Bl=BnJBq\Bq\BpVBoPBmEBmEBmEBi-Bh'Bg!B`�B_�Bc	BfBl@BtqBz�B}�B��B��B��B��B��B��B��B��B��B�B��B��B�B�B�&B�&B�&B�!B�B�8B�\B�]B�KB�uB�{B�{B�{B��B��B��B��B��B��B��B��B��B��B��B�B�B�%B�+B�7B�OB�[B�aB�aB�zBјBҞBӤBԪB׽B��B��B��B�B�B�#B�<B�SB�lB�}B��B��B��B��B	�B	�B	�B	B	 B	,B	9B	?B	KB	VB	cB	 iB	"uB	#{B	$�B	$�B	'�B	)�B	*�B	-�B	2�B	5�B	7�B	9�B	;B	<B	>B	?B	A*B	C6B	GNB	JaB	KfB	NxB	R�B	S�B	U�B	W�B	X�B	Z�B	\�B	^�B	a�B	g
B	k!B	m.B	qEB	rKB	qEB	rKB	tXB	wjB	xpB	xpB	xpB	z{B	|�B	�B	��B	��B	��B	��B	��B	��B	�B	�1B	�BB	�CB	�CB	�[B	�sB	�mB	�sB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�
B	��B	��B	�
B	�B	�
B	�B	�B	�
B	�B	�)B	�@B	�FB	�YB	�_B	�YB	�_B	�_B	�YB	�FB	�@B	�@B	�;B	�AB	�FB	�FB	�FB	�GB	�AB	�AB	�AB	�GB	�MB	�eB	�qB	�wB	ՕB	٭B	٭B	ڳB	۹B	۹B	ڳB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��G�O�B	��B	�iB
�B
�B
fB
#HB
,�B
3'B
7�B
>PB
C:B
I�B
N.B
R�B
W�B
[B
_,B
b�B
g^B
j�B
o111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111  <#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
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
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
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
PRES            TEMP            PSAL            PRES_ADJUSTED = PRES - dP                                                                                                                                                                                                                                       none                                                                                                                                                                                                                                                            PSAL_ADJUSTED = PSAL + dS, where dS is calculated from a potential conductivity (ref to 0 dbar) multiplicative adjustment factor r.                                                                                                                             dP =0.15 dbar.                                                                                                                                                                                                                                                  none                                                                                                                                                                                                                                                            r =0.9996(+/-0.0001), vertically averaged dS =-0.016(+/-0.003) in PSS-78.                                                                                                                                                                                       Pressures adjusted by using pressure offset at the sea surface. The quoted error is manufacturer specified accuracy in dbar.                                                                                                                                    The quoted error is manufacturer specified accuracy with respect to ITS-90 at time of laboratory calibration.                                                                                                                                                   Significant salinity drift detected. OWC least squares fit adopted. Salinity also adjusted for effects of pressure adjustment. The quoted error is max[0.01, 1xOWC uncertainty] in PSS-78.                                                                      202202041144302022020411443020220204114430  AO  ARCAADJP                                                                    20201028010207    IP                G�O�G�O�G�O�                AO  ARCAADJS                                                                    20201028010207    IP                G�O�G�O�G�O�                AO  ARGQQCPL                                                                    20201028010207  QCP$                G�O�G�O�G�O�5F03E           AO  ARGQQCPL                                                                    20201028010207  QCF$                G�O�G�O�G�O�4000            UW  ARSQUWQC    WOD & nearby Argo as visual check                               20220204114430  IP                  G�O�G�O�G�O�                