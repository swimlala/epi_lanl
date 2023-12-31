CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             
   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       2020-06-19T17:09:22Z creation      
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
_FillValue                    ��Argo profile    3.1 1.2 19500101000000  20200619170922  20220204114423  5905979 UW, Argo                                                        STEPHEN RISER                                                   PRES            TEMP            PSAL               wA   AO  7662                            2C  D   APEX                            8312                            080318                          846 @��݊�"1   @�����@5�7KƧ��c��S��1   GPS     Primary sampling: mixed [deeper than nominal 985dbar: discrete; nominal 985dbar to surface: 2dbar-bin averaged]                                                                                                                                                    wA   B   B   @���@�  A   A   A@  A`  A�  A�  A�  A�  A�  A�  A�  A�  B   B  B  B  B   B(  B0  B8  B@  BH  BP  BX  B`  Bh  Bp  Bx  B�  B�33B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�33B�  B�  B�  B�  B�  B�  B�  B�  B�  B�33B�  B�  B�  B�  B�  B�  B�  B�  C   C  C�C  C  C
  C  C  C  C  C  C  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.�C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq�fDr  Dr� Ds  Ds� Dt  Dt� Dt�fDy�\D��D�nD��qD���D��D�a�D���D���D�$)D�NfD���D��HD� �D�Y�Dڣ�D���D�#3D�T)D��D��=111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  @�  @�33@�33A��A=��A]��A}��A���A���A���A���A���A���A���A���BffBffBffBffB'ffB/ffB7ffB?ffBGffBOffBWffB_ffBgffBoffBwffBffB��fB��3B��3B��3B��3B��3B��3B��3B��3B��3B��3B��3B��fB��3B��3B��3Bó3Bǳ3B˳3Bϳ3Bӳ3B׳3B��fB߳3B�3B�3B�3B�3B�3B��3B��3B��3CٚC�4CٚCٚC	ٚCٚCٚCٚCٚCٚCٚCٚCٚCٚCٚCٚC!ٚC#ٚC%ٚC'ٚC)ٚC+ٚC-�4C/ٚC1ٚC3ٚC5ٚC7ٚC9ٚC;ٚC=ٚC?ٚCAٚCCٚCEٚCGٚCIٚCKٚCMٚCOٚCQٚCSٚCUٚCWٚCYٚC[ٚC]ٚC_ٚCaٚCcٚCeٚCgٚCiٚCkٚCmٚCoٚCqٚCsٚCuٚCwٚCyٚC{ٚC}ٚCٚC���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C�� C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���D vfD �fDvfD�fDvfD�fDvfD�fDvfD�fDvfD�fDvfD�fDvfD�fDvfD�fD	vfD	�fD
vfD
�fDvfD�fDvfD�fDvfD�fDvfD�fDvfD�fDvfD�fDvfD�fDvfD�fDvfD�fDvfD�fDvfD�fDvfD�fDvfD�fDvfD�fDvfD�fDvfD�fDvfD�fDvfD�fDvfD�fDvfD�fDvfD�fD vfD �fD!vfD!�fD"vfD"�fD#vfD#�fD$vfD$�fD%vfD%�fD&vfD&�fD'vfD'�fD(vfD(�fD)vfD)�fD*vfD*�fD+vfD+�fD,vfD,�fD-vfD-�fD.vfD.�fD/vfD/�fD0vfD0�fD1vfD1�fD2vfD2�fD3vfD3�fD4vfD4�fD5vfD5�fD6vfD6�fD7vfD7�fD8vfD8�fD9vfD9�fD:vfD:�fD;vfD;�fD<vfD<�fD=vfD=�fD>vfD>�fD?vfD?�fD@vfD@�fDAvfDA�fDBvfDB�fDCvfDC�fDDvfDD�fDEvfDE�fDFvfDF�fDGvfDG�fDHvfDH�fDIvfDI�fDJvfDJ�fDKvfDK�fDLvfDL�fDMvfDM�fDNvfDN�fDOvfDO�fDPvfDP�fDQvfDQ�fDRvfDR�fDSvfDS�fDTvfDT�fDUvfDU�fDVvfDV�fDWvfDW�fDXvfDX�fDYvfDY�fDZvfDZ�fD[vfD[�fD\vfD\�fD]vfD]�fD^vfD^�fD_vfD_�fD`vfD`�fDavfDa�fDbvfDb�fDcvfDc�fDdvfDd�fDevfDe�fDfvfDf�fDgvfDg�fDhvfDh�fDivfDi�fDjvfDj�fDkvfDk�fDlvfDl�fDmvfDm�fDnvfDn�fDovfDo�fDpvfDp�fDq|�Dq�fDrvfDr�fDsvfDs�fDtvfDt��Dy��D�)D�iGD���D���D�D�\�D��D��D�\D�I�D��)D��{D�)D�T�Dڟ
D��D�fD�O\D�D��p111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  @��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��Aҙ�Aҕ�Aҙ�Aҗ�Aҝ�Aқ�AҮAҝ�Aң�AҲ-AҴ9AҴ9AҲ-AҲ-AҴ9AҸRAҺ^AҾwA���A�A�ĜA�ĜA�ȴA�ȴA���A�Aҥ�A�x�A�/A���AѺ^AѺ^AѺ^AѺ^AѸRAѰ!Aѡ�Aѕ�Aч+A�x�A�r�A�p�A�`BA�I�A�r�A���A�VA�7LA�$�A�/A��^A�/A���A�I�A���A�JA���A�dZA�^5A�G�A�{A��A��PA��mA�~�A�1'A�33A�&�A�1'A���A��A�ȴA��A�-A�z�A�ZA�ȴA���A�bA�\)A�|�A��A��hA�VA�A�A�E�A�{A���A��A��A���A�-A��A�ĜA��A��A�A��\A���A��`A�ffA�-A��RA��A���A�r�A���A��TA�;dA���A��A���A�  A~��A|A�Aw�PAt�jAq?}Am�Al~�AlZAl(�Ak�Ai��AghsAd�/AaA_ƨA_��A_G�A^JA[VAW"�AU�AT=qAQ?}AOK�AM��AL�AJ��AJI�AI?}AG7LAC�#A@��A>1'A<�HA;�A:�!A933A6�jA4��A3K�A2ffA1O�A0$�A/K�A.��A-XA,�A+;dA*�A)?}A(��A(  A'��A'+A&A%�A$VA#��A#33A#A"~�A!�A bNA�PA=qA�+A+A��A��A�hA�mA��Ap�AE�Ap�A�;A�A9XA��A�RA
=A��A�A�+A  A��A�A��AhsA	;dAVAQ�A  A?}A��A �A�Az�At�AO�A7LA�AȴA��AXA �`A V@�^5@�?}@���@�1'@��@��@��w@�t�@���@�l�@�ff@�hs@��@�ƨ@���@�|�@�
=@�M�@�@�`B@�@柾@�Z@��@�1@�-@�Q�@ى7@ى7@׶F@�r�@���@Л�@���@��@�+@�C�@�K�@�^5@ͺ^@̃@��
@��@��@ɉ7@�Z@��/@��@�9X@Ǖ�@�C�@�v�@�j@���@�x�@��m@��@���@�1@�l�@�"�@��@�M�@�-@��h@��7@�O�@�%@��9@�b@�"�@�{@�  @��@���@�=q@�Q�@�"�@�t�@��m@�I�@��R@�|�@�$�@�@�7L@�1'@��@�@�ff@�$�@���@��@�X@�r�@�1'@�9X@�A�@�b@���@�S�@�o@�ȴ@���@�=q@�hs@�7L@�&�@�z�@���@���@�+@��R@�~�@�ff@�@��u@�Z@��m@��P@�K�@��H@��!@�^5@��@���@��7@�x�@�7L@���@��j@��@��u@�j@�9X@���@���@�\)@�;d@�@�^5@���@��#@���@��^@��7@�?}@��/@��D@��
@�ƨ@���@�ƨ@��F@��F@���@�l�@�;d@��@�
=@�l�@�;d@���@���@�n�@�-@��@��7@�O�@�/@��@��@���@���@���@��@�r�@�9X@��;@�ƨ@���@��P@���@��P@�\)@�"�@��@��y@��H@��R@�~�@�V@��@��@���@��^@���@�x�@�?}@��@���@��@�r�@�Z@�I�@�1'@� �@���@��w@��w@��@�C�@�"�@�
=@��@���@�E�@�5?@�5?@�5?@�$�@�{@��^@�X@��@�V@���@���@���@��9@��D@�bN@�9X@��@���@��m@��F@�+@�ȴ@���@�~�@�ff@�=q@�$�@�{@��@��#@��^@���@�x�@�X@�/@���@��j@��u@��u@�z�@�bN@�bN@�Z@�I�@�1'@��m@��P@�|�@�+@��H@�ȴ@��R@�!�@x7�@o4�@g@O@^��@W�@O��@J��@D��@?W?@7�m@1(�@*�M@&B[@!B�@�4@�8@[�@o @�W@oi111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111  Aҙ�Aҕ�Aҙ�Aҗ�Aҝ�Aқ�AҮAҝ�Aң�AҲ-AҴ9AҴ9AҲ-AҲ-AҴ9AҸRAҺ^AҾwA���A�A�ĜA�ĜA�ȴA�ȴA���A�Aҥ�A�x�A�/A���AѺ^AѺ^AѺ^AѺ^AѸRAѰ!Aѡ�Aѕ�Aч+A�x�A�r�A�p�A�`BA�I�A�r�A���A�VA�7LA�$�A�/A��^A�/A���A�I�A���A�JA���A�dZA�^5A�G�A�{A��A��PA��mA�~�A�1'A�33A�&�A�1'A���A��A�ȴA��A�-A�z�A�ZA�ȴA���A�bA�\)A�|�A��A��hA�VA�A�A�E�A�{A���A��A��A���A�-A��A�ĜA��A��A�A��\A���A��`A�ffA�-A��RA��A���A�r�A���A��TA�;dA���A��A���A�  A~��A|A�Aw�PAt�jAq?}Am�Al~�AlZAl(�Ak�Ai��AghsAd�/AaA_ƨA_��A_G�A^JA[VAW"�AU�AT=qAQ?}AOK�AM��AL�AJ��AJI�AI?}AG7LAC�#A@��A>1'A<�HA;�A:�!A933A6�jA4��A3K�A2ffA1O�A0$�A/K�A.��A-XA,�A+;dA*�A)?}A(��A(  A'��A'+A&A%�A$VA#��A#33A#A"~�A!�A bNA�PA=qA�+A+A��A��A�hA�mA��Ap�AE�Ap�A�;A�A9XA��A�RA
=A��A�A�+A  A��A�A��AhsA	;dAVAQ�A  A?}A��A �A�Az�At�AO�A7LA�AȴA��AXA �`A V@�^5@�?}@���@�1'@��@��@��w@�t�@���@�l�@�ff@�hs@��@�ƨ@���@�|�@�
=@�M�@�@�`B@�@柾@�Z@��@�1@�-@�Q�@ى7@ى7@׶F@�r�@���@Л�@���@��@�+@�C�@�K�@�^5@ͺ^@̃@��
@��@��@ɉ7@�Z@��/@��@�9X@Ǖ�@�C�@�v�@�j@���@�x�@��m@��@���@�1@�l�@�"�@��@�M�@�-@��h@��7@�O�@�%@��9@�b@�"�@�{@�  @��@���@�=q@�Q�@�"�@�t�@��m@�I�@��R@�|�@�$�@�@�7L@�1'@��@�@�ff@�$�@���@��@�X@�r�@�1'@�9X@�A�@�b@���@�S�@�o@�ȴ@���@�=q@�hs@�7L@�&�@�z�@���@���@�+@��R@�~�@�ff@�@��u@�Z@��m@��P@�K�@��H@��!@�^5@��@���@��7@�x�@�7L@���@��j@��@��u@�j@�9X@���@���@�\)@�;d@�@�^5@���@��#@���@��^@��7@�?}@��/@��D@��
@�ƨ@���@�ƨ@��F@��F@���@�l�@�;d@��@�
=@�l�@�;d@���@���@�n�@�-@��@��7@�O�@�/@��@��@���@���@���@��@�r�@�9X@��;@�ƨ@���@��P@���@��P@�\)@�"�@��@��y@��H@��R@�~�@�V@��@��@���@��^@���@�x�@�?}@��@���@��@�r�@�Z@�I�@�1'@� �@���@��w@��w@��@�C�@�"�@�
=@��@���@�E�@�5?@�5?@�5?@�$�@�{@��^@�X@��@�V@���@���@���@��9@��D@�bN@�9X@��@���@��m@��F@�+@�ȴ@���@�~�@�ff@�=q@�$�@�{@��@��#@��^@���@�x�@�X@�/@���@��j@��u@��u@�z�@�bN@�bN@�Z@�I�@�1'@��m@��P@�|�@�+@��H@�ȴG�O�@�!�@x7�@o4�@g@O@^��@W�@O��@J��@D��@?W?@7�m@1(�@*�M@&B[@!B�@�4@�8@[�@o @�W@oi111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111  ;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oG�O�;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oB
�+B
�1B
�+B
�B
�%B
�B
�1B
�%B
�%B
�7B
�=B
�=B
�7B
�7B
�=B
�=B
�DB
�PB
�VB
�\B
�\B
�\B
�bB
�oB
��B
��B
��B
��B
��B
�bB
�VB
�VB
�VB
�VB
�VB
�\B
�uB
��B
��B
��B
��B
��B
��B
��B
�LB
��BS�B@�BG�BXBgmB�7B�'B�B�B��B�'BB�)B�;B��BDB{BhBPBhBPB1B%BBBBB1B+B	7B
=BPB+BB��B��BVB�BJBB�B�wB�9B��B�-BɺBȴBÖB�qB��B�uB�DB{�BgmBbNB_;BO�B1'B�B  B
�fB
ÖB
��B
�+B
s�B
e`B
S�B
>wB
,B
%B	�B	�B	�wB	�9B	�-B	�B	�B	��B	�bB	�B	l�B	_;B	]/B	ZB	O�B	?}B	$�B	�B	�B	hB	\B	JB	+B	  B��B��B�B�TB��BÖB�wB�9B�3B�9B��B��B��B�{B�\B�VB�DB�=B�=B�%B�%B�B�B� B~�B|�B{�B{�Bz�By�Bx�B{�B|�B�B�B�B~�Bz�Bv�Br�Bl�BcTBgmBw�Bx�Bv�Br�Bp�Bm�Bl�Bq�Bu�Bq�BhsBy�B{�B|�B{�Bz�Bz�Bz�By�Bt�Bp�Bu�Bv�Bx�B{�Bz�By�Bv�Bu�Bu�Bv�Bv�Bv�Bw�Bt�Bt�Bs�Bs�Br�Br�Bq�Bs�Bs�Bq�Bp�Bq�Bs�Bt�Bu�Bv�Bw�Bv�Br�Bq�Bt�Bs�Br�Bq�Br�Bp�Bl�BffBaHB^5BXBdZB^5BT�BP�BO�BP�BQ�B\)BcTBhsBhsBl�Bl�Bm�Bk�Bl�Bo�Bo�Bw�B|�B}�B|�Bz�Bx�Bt�Bn�BjBhsBiyBiyBiyBjBjBk�Bo�Bv�Bz�Bz�Bz�Bz�Bz�Bz�Bz�B~�B�\B��B��B��B�{B��B��B��B��B�-B�B��B�B�'B�3B�9B�FB�LB�RB�dB��BŢBƨBŢBŢBǮB��B��B��B��B��B��B�B�#B�;B�HB�HB�HB�NB�`B�mB�mB�mB�B��B��B��B��B	  B	B	B	+B	DB	PB	VB	\B	hB	uB	{B	{B	{B	�B	�B	�B	�B	�B	�B	!�B	&�B	)�B	/B	1'B	33B	8RB	;dB	<jB	>wB	@�B	B�B	C�B	D�B	E�B	G�B	I�B	I�B	I�B	L�B	O�B	T�B	XB	YB	YB	YB	^5B	bNB	ffB	k�B	l�B	o�B	s�B	u�B	v�B	z�B	z�B	{�B	}�B	�B	�B	�B	�%B	�1B	�7B	�DB	�PB	�\B	�\B	�\B	�oB	�uB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�'B	�3B	�3B	�9B	�?B	�LB	�^B	�dB	�dB	�dB	�dB	�dB	�wB	��B	��B	��B	B	B	B	ÖB	ĜB	ŢB	ŢB	ƨB	ǮB	ɺB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�
B	�B	�B	�B	�B	�#B	�#B	�#B	�#B	�#B	�)B	�/B	�/B	�;B	�BB	�HB	�HB	�yB	�	B
�B
aB
B
(�B
9$B
=<B
B'B
ESB
K�B
P.B
V�B
Z�B
`B
c:B
g�B
i_B
p�B
v�B
zx111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111  B
{�B
|�B
{�B
y�B
z�B
y�B
|�B
z�B
z�B
}�B
~�B
~�B
}�B
}�B
~�B
~�B
�B
��B
��B
��B
��B
��B
��B
��B
��B
�B
�B
�B
��B
��B
��B
��B
��B
��B
��B
��B
��B
�B
�$B
�7B
�IB
�UB
�OB
�\B
��B
�BBHWB4�B<BLpB[�B}�B�B�sB�[B�IB��B��B�BӑB�HB��B�B�B�B�B�B��B�yB�sB�nB�aB�tB��B��B��B��B�B��B�]B�9B�RB�B�B �B�kB��B��B��B�'B��B�B�B��B��B�_B��B�BpOB[�BV�BS�BDKB%�B�B
�uB
��B
�B
�[B
{�B
h9B
Y�B
H~B
2�B
 �B	��B	�@B	̣B	�B	��B	��B	��B	��B	�QB	��B	x�B	a*B	S�B	Q�B	N�B	D�B	4!B	�B	YB	;B	B	B	 �B��B��B�B�B�FB�BƝB�IB�*B��B��B��B��B�|B�KB�4B�B�B�B~�B~�Bz�Bz�Bw�Bv�Bt�Bs�Bq�Bp�Bp�Bo�Bn�Bm�Bp�Bq�Bv�Bx�Bv�Bs�Bo�Bk�BgoBaKBXB\.Bl�Bm�Bk�BgpBeeBbRBaLBfkBj�BfkB]5Bn�Bp�Bq�Bp�Bo�Bo�Bo�Bn�Bi~BegBj�Bk�Bm�Bp�Bo�Bn�Bk�Bj�Bj�Bk�Bk�Bk�Bl�Bi�Bi�BhzBhzBgtBgtBfoBh{Bh{BfoBeiBfoBh{Bi�Bj�Bk�Bl�Bk�BgvBfpBi�Bh|BgvBfpBgvBekBaRB[.BVBR�BL�BY#BR�BI�BE�BD�BE�BF�BP�BXB]<B]=BaTBaTBbZB`OBaUBdgBdhBl�Bq�Br�Bq�Bo�Bm�Bi�BcbB_JB]>B^DB^EB^EB_KB_KB`QBdiBk�Bo�Bo�Bo�Bo�Bo�Bo�Bo�Bs�B�%B�hB��B�[B�DB�hB��B��B��B��B��B��B��B��B��B� B�B�B�B�*B�IB�gB�mB�hB�hB�sB��BBĤBƱBǷB��B��B��B��B�B�B�B�B�#B�0B�0B�0B�BB�~B�B�B�B��B��B��B��B	 B	B	B	B	(B	5B		;B		;B		;B	
AB	SB	ZB	eB	qB	}B	�B	�B	�B	#�B	%�B	'�B	-B	0!B	1'B	34B	5@B	7KB	8RB	9XB	:^B	<jB	>vB	>vB	>vB	A�B	D�B	I�B	L�B	M�B	M�B	M�B	R�B	WB	[ B	`>B	aDB	dWB	hnB	j{B	k�B	o�B	o�B	p�B	r�B	v�B	v�B	x�B	z�B	|�B	}�B	�B	�B	�B	�B	�B	�%B	�+B	�7B	�=B	�IB	�OB	�UB	�[B	�gB	�sB	�sB	�sB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�B	�B	�*B	�6B	�;B	�;B	�AB	�AB	�AB	�HB	�NB	�TB	�TB	�ZB	�`B	�lB	�sB	B	ĐB	ĐB	ŖB	ŖB	ƝB	ƝB	ǣB	ǣB	ǣB	ǣB	ȩB	ɯB	ɯB	ʵB	˻B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��G�O�B	�)B	��B	�vB
	B
�B
�B
-�B
1�B
6�B
9�B
@=B
D�B
KKB
OJB
T�B
W�B
\B
^	B
egB
krB
o!111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111  <#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
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
PRES            TEMP            PSAL            PRES_ADJUSTED = PRES - dP                                                                                                                                                                                                                                       none                                                                                                                                                                                                                                                            PSAL_ADJUSTED = PSAL + dS, where dS is calculated from a potential conductivity (ref to 0 dbar) multiplicative adjustment factor r.                                                                                                                             dP =0.15 dbar.                                                                                                                                                                                                                                                  none                                                                                                                                                                                                                                                            r =0.9997(+/-0.0001), vertically averaged dS =-0.011(+/-0.004) in PSS-78.                                                                                                                                                                                       Pressures adjusted by using pressure offset at the sea surface. The quoted error is manufacturer specified accuracy in dbar.                                                                                                                                    The quoted error is manufacturer specified accuracy with respect to ITS-90 at time of laboratory calibration.                                                                                                                                                   Significant salinity drift detected. OWC least squares fit adopted. Salinity also adjusted for effects of pressure adjustment. The quoted error is max[0.01, 1xOWC uncertainty] in PSS-78.                                                                      202202041144232022020411442320220204114423  AO  ARCAADJP                                                                    20200619170922    IP                G�O�G�O�G�O�                AO  ARGQQCPL                                                                    20200619170922  QCP$                G�O�G�O�G�O�5F03E           AO  ARGQQCPL                                                                    20200619170922  QCF$                G�O�G�O�G�O�0               UW  ARSQUWQC    WOD & nearby Argo as visual check                               20220204114423  IP                  G�O�G�O�G�O�                