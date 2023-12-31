CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             
   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       2018-06-24T17:02:30Z creation      
references        (http://www.argodatamgt.org/Documentation   comment       	free text      user_manual_version       3.2    Conventions       Argo-3.2 CF-1.6    featureType       trajectoryProfile      comment_dmqc_operator         (Matthew Alkire, University of Washington      @   	DATA_TYPE                  	long_name         	Data type      conventions       Argo reference table 1     
_FillValue                    6�   FORMAT_VERSION                 	long_name         File format version    
_FillValue                    6�   HANDBOOK_VERSION               	long_name         Data handbook version      
_FillValue                    6�   REFERENCE_DATE_TIME                 	long_name         !Date of reference for Julian days      conventions       YYYYMMDDHHMISS     
_FillValue                    6�   DATE_CREATION                   	long_name         Date of file creation      conventions       YYYYMMDDHHMISS     
_FillValue                    7   DATE_UPDATE                 	long_name         Date of update of this file    conventions       YYYYMMDDHHMISS     
_FillValue                    7   PLATFORM_NUMBER                   	long_name         Float unique identifier    conventions       WMO float identifier : A9IIIII     
_FillValue                    7,   PROJECT_NAME                  	long_name         Name of the project    
_FillValue                  @  74   PI_NAME                   	long_name         "Name of the principal investigator     
_FillValue                  @  7t   STATION_PARAMETERS           	            	long_name         ,List of available parameters for the station   conventions       Argo reference table 3     
_FillValue                  0  7�   CYCLE_NUMBER               	long_name         Float cycle number     conventions       =0...N, 0 : launch cycle (if exists), 1 : first complete cycle      
_FillValue         ��        7�   	DIRECTION                  	long_name         !Direction of the station profiles      conventions       -A: ascending profiles, D: descending profiles      
_FillValue                    7�   DATA_CENTRE                   	long_name         .Data centre in charge of float data processing     conventions       Argo reference table 4     
_FillValue                    7�   DC_REFERENCE                  	long_name         (Station unique identifier in data centre   conventions       Data centre convention     
_FillValue                     7�   DATA_STATE_INDICATOR                  	long_name         1Degree of processing the data have passed through      conventions       Argo reference table 6     
_FillValue                    8   	DATA_MODE                  	long_name         Delayed mode or real time data     conventions       >R : real time; D : delayed mode; A : real time with adjustment     
_FillValue                    8   PLATFORM_TYPE                     	long_name         Type of float      conventions       Argo reference table 23    
_FillValue                     8   FLOAT_SERIAL_NO                   	long_name         Serial number of the float     
_FillValue                     88   FIRMWARE_VERSION                  	long_name         Instrument firmware version    
_FillValue                     8X   WMO_INST_TYPE                     	long_name         Coded instrument type      conventions       Argo reference table 8     
_FillValue                    8x   JULD               	long_name         ?Julian day (UTC) of the station relative to REFERENCE_DATE_TIME    standard_name         time   units         "days since 1950-01-01 00:00:00 UTC     conventions       8Relative julian days with decimal part (as parts of day)   
resolution        >�EȠ      
_FillValue        A.�~       axis      T           8|   JULD_QC                	long_name         Quality on date and time   conventions       Argo reference table 2     
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
_FillValue                    �|   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  �|   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  0  �t   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                    ��   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                    ��   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                    ��   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  ,  ��   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                    ��   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                    ��   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                    ��   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                    ��   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  @  ��   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                    �    HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                    �0   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    �4   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar         �D   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar         �H   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�        �L   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    �PArgo profile    3.1 1.2 19500101000000  20180624170230  20190604094146  5903736 US ARGO PROJECT                                                 STEPHEN RISER                                                   PRES            TEMP            PSAL               �A   AO  4051                            2C  D   APEX                            5368                            041511                          846 @�l��1   @�l�E7|@5��G�{�d���v�1   GPS     Primary sampling: mixed [deeper than nominal 985dbar: discrete; nominal 985dbar to surface: 2dbar-bin averaged]                                                                                                                                                    �A   B   B   @�ff@�  A   A   A@  Aa��A{33A�  A�  A�  A�  A�  A�  A�  B   B  B  B  B   B(  B0  B8  B@  BH  BP  BX  B`  Bh  Bp  Bx  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  C   C  C  C  C  C
  C  C  C  C  C  C  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C��C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO�fDP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dt� Dt��Dy��D�"�D�<{D���D�ǮD��D�L)D��qD���D� D�>D�~fD��3D��D�3�Dڋ�DྸD�
=D�G�D�p�D���111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  @���@�33@�33A��A=��A_34Ax��A���A���A���A���A���A���A���A���BffBffBffBffB'ffB/ffB7ffB?ffBGffBOffBWffB_ffBgffBoffBwffBffB��3B��3B��3B��3B��3B��3B��3B��3B��3B��3B��3B��3B��3B��3B��3B��3Bó3Bǳ3B˳3Bϳ3Bӳ3B׳3B۳3B߳3B�3B�3B�3B�3B�3B��3B��3B��3CٚCٚCٚCٚC	ٚCٚCٚCٚCٚCٚCٚCٚCٚCٚCٚCٚC!ٚC#ٚC%ٚC'ٚC)ٚC+ٚC-ٚC/ٚC1ٚC3ٚC5ٚC7ٚC9ٚC;ٚC=ٚC?ٚCAٚCCٚCEٚCGٚCIٚCKٚCMٚCOٚCQٚCSٚCUٚCWٚCYٚC[ٚC]ٚC_ٚCaٚCcٚCeٚCgٚCiٚCkٚCmٚCoٚCqٚCsٚCuٚCwٚCyٚC{ٚC}ٚCٚC���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���D vfD �fDvfD�fDvfD�fDvfD�fDvfD�fDvfD�fDvfD�fDvfD�fDvfD�fD	vfD	�fD
vfD
�fDvfD�fDvfD�fDvfD�fDvfD�fDvfD�fDvfD�fDvfD�fDvfD�fDvfD�fDvfD�fDvfD�fDvfD�fDvfD�fDvfD�fDvfD�fDvfD�fDvfD�fDvfD�fDvfD�fDvfD�fDvfD�fD vfD �fD!vfD!�fD"vfD"�fD#vfD#�fD$vfD$�fD%vfD%�fD&vfD&�fD'vfD'�fD(vfD(�fD)vfD)�fD*vfD*�fD+vfD+�fD,vfD,�fD-vfD-�fD.vfD.�fD/vfD/�fD0vfD0�fD1vfD1�fD2vfD2�fD3vfD3�fD4vfD4�fD5vfD5�fD6vfD6�fD7vfD7�fD8vfD8�fD9vfD9�fD:vfD:�fD;vfD;�fD<vfD<�fD=vfD=�fD>vfD>�fD?vfD?�fD@vfD@�fDAvfDA�fDBvfDB�fDCvfDC�fDDvfDD�fDEvfDE�fDFvfDF�fDGvfDG�fDHvfDH�fDIvfDI�fDJvfDJ�fDKvfDK�fDLvfDL�fDMvfDM�fDNvfDN�fDO|�DO�fDPvfDP�fDQvfDQ�fDRvfDR�fDSvfDS�fDTvfDT�fDUvfDU�fDVvfDV�fDWvfDW�fDXvfDX�fDYvfDY�fDZvfDZ�fD[vfD[�fD\vfD\�fD]vfD]�fD^vfD^�fD_vfD_�fD`vfD`�fDavfDa�fDbvfDb�fDcvfDc�fDdvfDd�fDevfDe�fDfvfDf�fDgvfDg�fDhvfDh�fDivfDi�fDjvfDj�fDkvfDk�fDlvfDl�fDmvfDm�fDnvfDn�fDovfDo�fDpvfDp�fDqvfDq�fDrvfDr�fDsvfDs�fDtvfDt�3Dy��D�D�7�D���D���D��D�G\D���D�� D�3D�9GD�y�D��fD� D�.�Dڇ
D��D�pD�B�D�k�D���111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  @��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��A��A��A�{A�oA�bA��yA��A���A�bAȴ9A�O�A���AƉ7A�1'Aź^A�r�A�E�A�M�A�VA�-A� �A��mA�r�A���A���A��7A� �A��A�5?A��RA��DA�t�A�bNA�A�A��A�  A��A�x�A�1'A��A�~�A�&�A���A���A��+A�;dA��A��hA���A�dZA��`A��A���A�
=A��RA�t�A�O�A�~�A�A�ffA��yA���A�M�A�+A�O�A���A��uA�1A���A�
=A�^5A��A�l�A�7LA�C�A��A��;A�-A�A�A��9A��DA�A�A��A�M�A� �A�1A�ĜA�-A�t�A��uA��+A���A�ĜA���A��A�;dA�O�A��
A��;A�(�A�"�A�-A���A�-A���A���A��A�5?A��A�VA�bA�{A�ZA���A��A�r�A�9XA~jA|ĜA{"�Ax��Av-AtȴAr��Aq��Ap��Am�wAm`BAl��AjȴAi�FAg\)Af9XAe%Ac;dA`5?A^��A\��AZ��AZ�AZ5?AW��AT�DAShsAO�wAM"�AL�AK|�AI�wAI�AI�^AIAH^5AGK�AEt�AD5?ACS�ABjAB�AA��A@�!A?�A>��A=33A;�A:��A:ffA8r�A5?}A4  A2��A0��A/`BA/;dA.�+A-�A-�A-t�A-C�A-�A,E�A)�TA&VA$�/A$�A$^5A$=qA#��A#��A#?}A"��A!��A!�7A!�A v�AdZAI�A��AC�A\)A
=A��A�A��A�hA��A�RA�A/A�HA��A33A|�A?}A{A��A7LAA�9AI�A�A	��A	;dA��A�wA7LAbA��A ��@�Z@��@���@��\@�x�@�V@��`@���@�b@���@�G�@�bN@�&�@@�t�@��H@�u@��@�M�@�5?@蛦@�\@�-@��@�1'@���@���@�b@��@�{@�`B@܋D@�S�@���@�V@���@�bN@��@Ցh@���@�bN@�Q�@�A�@�  @���@��@ѩ�@�%@Ь@�|�@͡�@��@���@˝�@�K�@�n�@�V@�r�@���@ũ�@Ł@�G�@��
@�$�@�G�@�z�@�1'@�b@���@�l�@��R@�-@�&�@�1@�;d@��R@�M�@��@�@��@��^@�p�@�%@�A�@�\)@��h@��
@���@�E�@�~�@��R@�ȴ@�ff@�J@�X@���@�Ĝ@��j@���@�r�@�1@��R@��H@�@�"�@�
=@��\@�n�@���@�{@�x�@��9@��@���@�@��R@�~�@�=q@��@�J@�J@���@���@���@��@���@�X@�V@��D@�9X@�ƨ@��F@���@�\)@�+@�
=@��@��@���@���@�M�@�x�@��@���@�Q�@���@���@�E�@��@��#@�hs@��@��@��/@��@�z�@�j@�A�@���@��@�;d@��H@���@�v�@�ff@�{@���@���@���@�p�@�?}@��@��@�Ĝ@��9@�9X@��w@�+@��@���@�n�@�E�@�$�@���@�X@�G�@�V@���@�bN@�9X@��@��@���@��@�l�@�"�@��y@�ȴ@�ff@�$�@��T@���@��@�G�@��@���@��/@���@���@��9@�r�@�Z@�I�@�(�@��;@��@��@�C�@�o@��y@���@���@��+@�V@���@���@�7L@��`@��9@���@��u@��@�r�@�Z@�I�@�A�@�(�@� �@� �@�b@�  @��@�|�@�33@�
=@���@���@�J@���@���@��@�&�@���@�bN@��@��m@��F@���@�dZ@���@�@���@��	@{.I@t��@k� @c@O@X�?@R=q@J �@B�]@=(�@5X@.ȴ@)5�@#�@RT@�9@��@kQ@h
111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111  A��A��A�{A�oA�bA��yA��A���A�bAȴ9A�O�A���AƉ7A�1'Aź^A�r�A�E�A�M�A�VA�-A� �A��mA�r�A���A���A��7A� �A��A�5?A��RA��DA�t�A�bNA�A�A��A�  A��A�x�A�1'A��A�~�A�&�A���A���A��+A�;dA��A��hA���A�dZA��`A��A���A�
=A��RA�t�A�O�A�~�A�A�ffA��yA���A�M�A�+A�O�A���A��uA�1A���A�
=A�^5A��A�l�A�7LA�C�A��A��;A�-A�A�A��9A��DA�A�A��A�M�A� �A�1A�ĜA�-A�t�A��uA��+A���A�ĜA���A��A�;dA�O�A��
A��;A�(�A�"�A�-A���A�-A���A���A��A�5?A��A�VA�bA�{A�ZA���A��A�r�A�9XA~jA|ĜA{"�Ax��Av-AtȴAr��Aq��Ap��Am�wAm`BAl��AjȴAi�FAg\)Af9XAe%Ac;dA`5?A^��A\��AZ��AZ�AZ5?AW��AT�DAShsAO�wAM"�AL�AK|�AI�wAI�AI�^AIAH^5AGK�AEt�AD5?ACS�ABjAB�AA��A@�!A?�A>��A=33A;�A:��A:ffA8r�A5?}A4  A2��A0��A/`BA/;dA.�+A-�A-�A-t�A-C�A-�A,E�A)�TA&VA$�/A$�A$^5A$=qA#��A#��A#?}A"��A!��A!�7A!�A v�AdZAI�A��AC�A\)A
=A��A�A��A�hA��A�RA�A/A�HA��A33A|�A?}A{A��A7LAA�9AI�A�A	��A	;dA��A�wA7LAbA��A ��@�Z@��@���@��\@�x�@�V@��`@���@�b@���@�G�@�bN@�&�@@�t�@��H@�u@��@�M�@�5?@蛦@�\@�-@��@�1'@���@���@�b@��@�{@�`B@܋D@�S�@���@�V@���@�bN@��@Ցh@���@�bN@�Q�@�A�@�  @���@��@ѩ�@�%@Ь@�|�@͡�@��@���@˝�@�K�@�n�@�V@�r�@���@ũ�@Ł@�G�@��
@�$�@�G�@�z�@�1'@�b@���@�l�@��R@�-@�&�@�1@�;d@��R@�M�@��@�@��@��^@�p�@�%@�A�@�\)@��h@��
@���@�E�@�~�@��R@�ȴ@�ff@�J@�X@���@�Ĝ@��j@���@�r�@�1@��R@��H@�@�"�@�
=@��\@�n�@���@�{@�x�@��9@��@���@�@��R@�~�@�=q@��@�J@�J@���@���@���@��@���@�X@�V@��D@�9X@�ƨ@��F@���@�\)@�+@�
=@��@��@���@���@�M�@�x�@��@���@�Q�@���@���@�E�@��@��#@�hs@��@��@��/@��@�z�@�j@�A�@���@��@�;d@��H@���@�v�@�ff@�{@���@���@���@�p�@�?}@��@��@�Ĝ@��9@�9X@��w@�+@��@���@�n�@�E�@�$�@���@�X@�G�@�V@���@�bN@�9X@��@��@���@��@�l�@�"�@��y@�ȴ@�ff@�$�@��T@���@��@�G�@��@���@��/@���@���@��9@�r�@�Z@�I�@�(�@��;@��@��@�C�@�o@��y@���@���@��+@�V@���@���@�7L@��`@��9@���@��u@��@�r�@�Z@�I�@�A�@�(�@� �@� �@�b@�  @��@�|�@�33@�
=@���@���@�J@���@���@��@�&�@���@�bN@��@��m@��F@���@�dZG�O�@�@���@��	@{.I@t��@k� @c@O@X�?@R=q@J �@B�]@=(�@5X@.ȴ@)5�@#�@RT@�9@��@kQ@h
111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111  ;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oG�O�;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oB�oB�uB�uB�uB�uB�uB��B�`B�B  BBuB49B>wBC�BC�BB�BD�BI�BH�BG�BE�BF�B[#B^5B_;BaHBjBq�B{�B{�B|�B|�B|�B|�B}�B� B�B�B�1B�PB�hB��B��B��B��B��B��B��B�B�'B�RB�}B�}B�}B�}B�wB�wB�jB�dB�^B�RB�FB�LB�FB�?B�3B�!B�B��B��B��B��B�VB�B|�Bt�Bm�BcTB\)BR�BH�BE�B@�B5?B)�B#�B\B��B�sB�
B�qB��B�B�B��B��B� B`BBG�B9XB-B"�B�BoBB
��B
�B
�`B
�BB
�#B
��B
B
�XB
��B
��B
�B
s�B
dZB
VB
D�B
0!B
#�B
�B
VB
B	��B
  B	��B	�B	�B	�`B	�/B	��B	ƨB	�-B	��B	��B	�hB	�PB	�+B	p�B	W
B	K�B	6FB	+B	&�B	&�B	'�B	'�B	'�B	.B	0!B	+B	!�B	�B	�B	�B	�B	�B	VB	1B	  B��B�mB�ZB�;B��BB�dB�FB�9B�'B�!B�!B�B�B�B�B�B��B��B��B��B��B��B��B�{B�uB�oB�bB�\B�VB�PB�DB�1B�+B�%B�B�B�B�B�B�B�B�B�DB�hB��B��B��B��B��B�uB�bB�hB�hB�\B�JB�=B�1B�1B�+B�Bv�Bl�BhsBk�Bk�BgmBt�Bw�Bu�B�B�B�B�B�B�%B�+B�%B�7B�DB�=B�=B�JB�JB�PB�JB�VB�hB�oB�hB�bB�VB�JB�=B�DB�JB�PB�PB�VB�JB�DB�=B�7B�=B�\B��B��B��B��B��B��B��B��B��B��B��B�B�-B�3B�3B�9B�?B�RB�RB�qBĜBĜBŢB��B��B��B��B��B��B��B��B��B��B�B�B�B�#B�/B�5B�5B�BB�TB�fB�sB�B�B�B�B�B��B��B��B	B	B	%B	B	+B		7B	
=B	JB	\B	\B	�B	�B	�B	�B	�B	�B	�B	%�B	+B	-B	1'B	5?B	8RB	<jB	=qB	=qB	>wB	@�B	@�B	B�B	C�B	D�B	E�B	E�B	E�B	F�B	G�B	K�B	M�B	Q�B	Q�B	Q�B	S�B	VB	VB	W
B	W
B	YB	XB	YB	ZB	YB	XB	YB	ZB	_;B	aHB	bNB	cTB	ffB	gmB	iyB	iyB	m�B	q�B	q�B	r�B	t�B	x�B	z�B	}�B	� B	�B	�B	�B	�%B	�7B	�7B	�DB	�JB	�\B	�\B	�\B	�bB	�hB	�uB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�'B	�3B	�?B	�FB	�RB	�dB	�wB	�}B	��B	ÖB	ĜB	ƨB	ǮB	ȴB	ɺB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�#B	�/B	�5B	�;B	�BB	�BB	�BB	�HB	�NB	�NB	�TB	�TB	�TB	�TB	�ZB	�fB	�mB	�yB	�yB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B
�B
QB
�B
&�B
1AB
;B
A�B
H�B
MB
O�B
V9B
X�B
`B
gB
j�B
l�B
poB
s�B
v�B
{d111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111  B�B�B�B�B�B�B�{B��B�3B�B�BB"�B-B2'B2"B1B3'B8CB7CB6;B4/B55BI�BL�BM�BO�BYB`8BjmBjwBkrBktBkvBkuBlwBn�Bp�Bs�Bv�B{�B�B�B�B�B�'B�3B�CB�vB��B��B��B� B��B��B� B��B��B��B��B��B��B��B��B��B��B��B��B��B��B�bB�<B�"B|�Br�Bk}BcNB\"BQ�BJ�BA�B7OB48B/B#�B�BuB��B�mB�BűB�!B��B��B��B��B�LBn�BN�B6oB(B�B�B	bB:B
��B
�B
�YB
�0B
�B
��B
��B
�dB
�/B
��B
�^B
r�B
b�B
SBB
D�B
3�B
	B
�B
{B	�DB	�B	��B	��B	��B	�B	܇B	�XB	�,B	��B	��B	�-B	��B	��B	�mB	|UB	v4B	_�B	FB	:�B	%YB	B	 B	�B		B	B	B	,B	8B	B	�B		�B	�B	�B	�B	�B�rB�PB�!B��B֐B�|B�^B�B��B��B�rB�gB�RB�NB�LB�HB�AB�AB�7B�1B�B��B��B��B��B��B��B��B��B��B�B~�B}�B|�BzyBwdBvaBu[BsOBrGBqCBqDBqEBp>Bp:BtVBzzB��B��B��B��B��B��B��B�B��B��B~�B{�ByuBwmBwhBvfBpDBfB[�BW�BZ�BZ�BV�Bc�BgBeBpDBsWBsVBt]Bt\Bu`BvjBucBxxBz�ByzBy|B{�B{�B|�B{�B}�B��B��B��B�B}�B{�By}Bz�B{�B|�B|�B}�B{�Bz�By|BxwBy~B~�B��B��B��B��B�B�B�$B�'B�4B�4B�;B�RB�lB�sB�rB�tB�|B��B��B��B��B��B��B��B�B�B�(B�/B�(B�&B�.B�;B�8B�AB�LB�QB�_B�kB�qB�oB�zBҍB՞B׬B��B��B��B��B��B��B�B�,B�=B�UB�ZB�SB�\B�rB�sB�B��B��B	�B	�B	
�B	�B	�B	�B	�B	B	5B	AB	 ZB	$pB	'�B	+�B	,�B	,�B	-�B	/�B	/�B	1�B	2�B	3�B	4�B	4�B	4�B	5�B	6�B	:�B	=B	AB	AB	AB	C$B	E2B	E3B	F7B	F8B	HHB	G?B	HHB	IKB	HBB	G?B	HFB	IKB	NgB	PrB	Q|B	R�B	U�B	V�B	X�B	X�B	\�B	`�B	`�B	a�B	c�B	g�B	jB	mB	o*B	p1B	q1B	sAB	uQB	x`B	x^B	zlB	{rB	~�B	~�B	~�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�B	�B	� B	�*B	�+B	�4B	�BB	�LB	�UB	�eB	�lB	�tB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�B	�B	�"B	�/B	�DB	�QB	�TB	�ZB	�eB	�^B	�dB	�gB	�oB	�kB	�rB	�tB	�qB	�pB	�uB	ՂB	։B	ؙB	ؗB	ٛB	ٛB	ڢB	ۨB	۩B	ܮB	ݷB	޿B	��B	��B	��B	��B	��B	��G�O�B	��B	�B
	mB
�B
B
 XB
*�B
0�B
7�B
<B
>�B
EXB
G�B
O#B
V/B
Y�B
[�B
_�B
b�B
e�B
j}111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111  <#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
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
PRES            TEMP            PSAL            PRES_ADJUSTED = PRES - dP                                                                                                                                                                                                                                       none                                                                                                                                                                                                                                                            PSAL_ADJUSTED = PSAL + dS, where dS is calculated from a potential conductivity (ref to 0 dbar) multiplicative adjustment factor r.                                                                                                                             dP =0.15 dbar.                                                                                                                                                                                                                                                  none                                                                                                                                                                                                                                                            r =0.9996(+/-0.0001), vertically averaged dS =-0.017(+/-0.003) in PSS-78.                                                                                                                                                                                       Pressures adjusted by using pressure offset at the sea surface. The quoted error is manufacturer specified accuracy in dbar.                                                                                                                                    The quoted error is manufacturer specified accuracy with respect to ITS-90 at time of laboratory calibration.                                                                                                                                                   Significant salinity drift detected. OWC least squares fit adopted. Map scales: x=6,3; y=2,1. Salinity also adjusted for effects of pressure adjustment. The quoted error is max[0.01, 1xOWC uncertainty] in PSS-78.                                            201906040941462019060409414620190604094146  AO  ARCAADJP                                                                    20180624170230    IP                G�O�G�O�G�O�                AO  ARGQQCPL                                                                    20180624170230  QCP$                G�O�G�O�G�O�5F03E           AO  ARGQQCPL                                                                    20180624170230  QCF$                G�O�G�O�G�O�0               UW  ARSQUWQC    WOD & nearby Argo as visual check                               20190604094146  IP                  G�O�G�O�G�O�                