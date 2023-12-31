CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             
   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       2018-11-21T04:11:51Z creation      
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
_FillValue                 �  A�   PRES_ADJUSTED            
      	   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  C�   PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  K�   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         decibar    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  M�   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  U�   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ]t   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  _p   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  g`   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         degree_Celsius     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  i\   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  qL   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  y<   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  {8   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �(   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  �$   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  0  �   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                    �D   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                    �D   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                    �D   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  ,  �D   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                    �p   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                    �t   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                    �x   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                    �|   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  @  ��   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                    ��   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                    ��   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    ��   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar         ��   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar         ��   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�        ��   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    ��Argo profile    3.1 1.2 19500101000000  20181121041151  20190604094021  5903736 US ARGO PROJECT                                                 STEPHEN RISER                                                   PRES            TEMP            PSAL               �A   AO  4051                            2C  D   APEX                            5368                            041511                          846 @׹Pv���1   @׹Q���@2���n��d-�^5?}1   GPS     Primary sampling: mixed [deeper than nominal 985dbar: discrete; nominal 985dbar to surface: 2dbar-bin averaged]                                                                                                                                                    �A   B   B   @���@�  @���A   A@  A`  A�  A�  A�  A�  A�  A�  A�  A�  B   B  B  B  B   B(  B0  B8  B@ffBHffBP  BX  B`  Bh  Bp  Bx  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  C   C  C  C  C  C
  C  C  C  C  C  C  C  C  C  C�C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dtl�Dy��D��D�B�D�~D��\D�D�  D��=D���D�qD�:�D�xRD��qD���D�0�Dڛ�D��\D��3D�.�D��1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111@�  @�33@�  A��A=��A]��A}��A���A���A���A���A���A���A���A���BffBffBffBffB'ffB/ffB7ffB?��BG��BOffBWffB_ffBgffBoffBwffBffB��3B��3B��3B��3B��3B��3B��3B��3B��3B��3B��3B��3B��3B��3B��3B��3Bó3Bǳ3B˳3Bϳ3Bӳ3B׳3B۳3B߳3B�3B�3B�3B�3B�3B��3B��3B��3CٚCٚCٚCٚC	ٚCٚCٚCٚCٚCٚCٚCٚCٚCٚC�4CٚC!ٚC#ٚC%ٚC'ٚC)ٚC+ٚC-ٚC/ٚC1ٚC3ٚC5ٚC7ٚC9ٚC;ٚC=ٚC?ٚCAٚCCٚCEٚCGٚCIٚCKٚCMٚCOٚCQٚCSٚCUٚCWٚCYٚC[ٚC]ٚC_ٚCaٚCcٚCeٚCgٚCiٚCkٚCmٚCoٚCqٚCsٚCuٚCwٚCyٚC{ٚC}ٚCٚC���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���D vfD �fDvfD�fDvfD�fDvfD�fDvfD�fDvfD�fDvfD�fDvfD�fDvfD�fD	vfD	�fD
vfD
�fDvfD�fDvfD�fDvfD�fDvfD�fDvfD�fDvfD�fDvfD�fDvfD�fDvfD�fDvfD�fDvfD�fDvfD�fDvfD�fDvfD�fDvfD�fDvfD�fDvfD�fDvfD�fDvfD�fDvfD�fDvfD�fD vfD �fD!vfD!�fD"vfD"�fD#vfD#�fD$vfD$�fD%vfD%�fD&vfD&�fD'vfD'�fD(vfD(�fD)vfD)�fD*vfD*�fD+vfD+�fD,vfD,�fD-vfD-�fD.vfD.�fD/vfD/�fD0vfD0�fD1vfD1�fD2vfD2�fD3vfD3�fD4vfD4�fD5vfD5�fD6vfD6�fD7vfD7�fD8vfD8�fD9vfD9�fD:vfD:�fD;vfD;�fD<vfD<�fD=vfD=�fD>vfD>�fD?vfD?�fD@vfD@�fDAvfDA�fDBvfDB�fDCvfDC�fDDvfDD�fDEvfDE�fDFvfDF�fDGvfDG�fDHvfDH�fDIvfDI�fDJvfDJ�fDKvfDK�fDLvfDL�fDMvfDM�fDNvfDN�fDOvfDO�fDPvfDP�fDQvfDQ�fDRvfDR�fDSvfDS�fDTvfDT�fDUvfDU�fDVvfDV�fDWvfDW�fDXvfDX�fDYvfDY�fDZvfDZ�fD[vfD[�fD\vfD\�fD]vfD]�fD^vfD^�fD_vfD_�fD`vfD`�fDavfDa�fDbvfDb�fDcvfDc�fDdvfDd�fDevfDe�fDfvfDf�fDgvfDg�fDhvfDh�fDivfDi�fDjvfDj�fDkvfDk�fDlvfDl�fDmvfDm�fDnvfDn�fDovfDo�fDpvfDp�fDqvfDq�fDrvfDr�fDsvfDs�fDtc3Dy�GD���D�>D�yGD�ҏD�RD��3D��pD��
D��D�6D�s�D�ؤD���D�+�Dږ�D�ʏD��fD�)�D�|)1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��A��A��A�"�A�33A�5?A��A�  A��A��yA��`A��
AؼjA�ĜA�A؛�A؍PA�K�A�E�A�E�A�C�A�?}A�?}A�?}A�=qA�;dA�9XA�5?A�5?A��A�=qA�p�A�n�A�~�A���Aѥ�A�XA�?}A���A���AГuA�^5A��A��
AϏ\A�t�A��AΙ�A�G�A�A�S�A���A�G�A�$�A��A˝�A�7LA��;AʃA�/A�Aɡ�A��A�ƨAȇ+A��A�I�AƮAƛ�A�A�&�AĴ9A�O�A��A×�A��mA��A��DA���A�1A��jA�n�A��#A�t�A�z�A�C�A�/A�A��9A��#A�%A�I�A�+A��A�^5A�l�A�ƨA�%A���A���A���A�\)A��wA�XA�^5A�bA�9XA�~�A��A��TA�9XA�{A��RA���A�33A�O�A���A�C�A��yA��A��!A��FA��\A�JA�E�A� �A~��A|  Ay�Av��Arr�ApA�Am��AiXAfbNAet�Ad�Ab9XA`��A_C�A\�!A[;dAY�TAVȴAU\)ATr�AQl�AN�AL�`AKC�AH�jAF��AE&�AC�AB�jAAA@I�A>��A=|�A<��A;�A9`BA6�A5�hA3��A2��A2  A.5?A-"�A+�hA*�uA)\)A(�DA'��A'��A'�7A&�A&=qA$��A#A"JA ffAƨA~�A�DA9XA�-A��A�#A��A^5A�-A��A�DA�A�A=qA��A��A�;A  AI�Ap�AoA
I�A	33A�Av�A�A�DA  A�A��AZA�wA �RA -@���@�$�@�?}@��@��y@���@�?}@�9X@�K�@�x�@�7L@�;d@�z�@�33@�v�@��@�7@�1@�+@�h@���@�h@�|�@��@��@�I�@�(�@�t�@���@�^5@��H@�E�@؋D@�C�@�x�@��@�r�@���@���@���@̋D@�C�@��#@�hs@��`@�1'@�t�@�+@�ȴ@Ł@�  @�\)@�@��@�v�@°!@�-@�7L@���@���@�"�@�J@���@�%@��@�b@�l�@��R@�@��@��h@���@��j@�  @�;d@�ȴ@��+@��@��@�O�@��@�I�@��F@�l�@�;d@��H@�5?@�7L@���@���@���@���@�A�@���@�K�@�o@�n�@�$�@��@��#@�`B@���@� �@��@��
@��@�S�@�
=@��\@�{@���@�G�@��@���@��9@�z�@� �@��
@��F@���@���@�|�@�33@�v�@��#@�&�@�Ĝ@�Ĝ@��@��u@��D@�bN@��@��F@��@�K�@��@���@�~�@�ff@��@���@��-@�`B@�&�@��@��u@�r�@�1'@��@��P@�t�@�dZ@�S�@�o@��H@��+@�n�@�-@�{@��^@��@�/@�Q�@�9X@�9X@�A�@��F@�\)@�K�@�
=@���@��\@�ff@�^5@�V@�-@�{@�J@�@���@���@�X@�&�@�&�@��u@�(�@�(�@��@�"�@��@���@�5?@��7@��@��`@��u@��@�(�@���@�\)@�@���@���@���@���@���@��\@�5?@�J@�^5@���@�ȴ@���@��@�+@��@��H@��+@�E�@���@��@��^@�X@���@���@���@�Z@��w@���@�\)@�
=@��@��@���@���@�5?@���@�X@��@���@��`@�r�@���@���@��@�l�@�;d@�"�@�+@�
=@�ȴ@��\@�E�@�{@�@���@���@���@��h@�O�@��@��/@���@�1'@�1@���@��
@��w@�dZ@�o@��@�N<@~h
@w"�@pG@dr�@[|�@U\�@L(�@C��@=@@7�w@1��@*u@#�]@��@B�@ϫ@c @�@
�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111411111111111111111111A��A��A�"�A�33A�5?A��A�  A��A��yA��`A��
AؼjA�ĜA�A؛�A؍PA�K�A�E�A�E�A�C�A�?}A�?}A�?}A�=qA�;dA�9XA�5?A�5?A��A�=qA�p�A�n�A�~�A���Aѥ�A�XA�?}A���A���AГuA�^5A��A��
AϏ\A�t�A��AΙ�A�G�A�A�S�A���A�G�A�$�A��A˝�A�7LA��;AʃA�/A�Aɡ�A��A�ƨAȇ+A��A�I�AƮAƛ�A�A�&�AĴ9A�O�A��A×�A��mA��A��DA���A�1A��jA�n�A��#A�t�A�z�A�C�A�/A�A��9A��#A�%A�I�A�+A��A�^5A�l�A�ƨA�%A���A���A���A�\)A��wA�XA�^5A�bA�9XA�~�A��A��TA�9XA�{A��RA���A�33A�O�A���A�C�A��yA��A��!A��FA��\A�JA�E�A� �A~��A|  Ay�Av��Arr�ApA�Am��AiXAfbNAet�Ad�Ab9XA`��A_C�A\�!A[;dAY�TAVȴAU\)ATr�AQl�AN�AL�`AKC�AH�jAF��AE&�AC�AB�jAAA@I�A>��A=|�A<��A;�A9`BA6�A5�hA3��A2��A2  A.5?A-"�A+�hA*�uA)\)A(�DA'��A'��A'�7A&�A&=qA$��A#A"JA ffAƨA~�A�DA9XA�-A��A�#A��A^5A�-A��A�DA�A�A=qA��A��A�;A  AI�Ap�AoA
I�A	33A�Av�A�A�DA  A�A��AZA�wA �RA -@���@�$�@�?}@��@��y@���@�?}@�9X@�K�@�x�@�7L@�;d@�z�@�33@�v�@��@�7@�1@�+@�h@���@�h@�|�@��@��@�I�@�(�@�t�@���@�^5@��H@�E�@؋D@�C�@�x�@��@�r�@���@���@���@̋D@�C�@��#@�hs@��`@�1'@�t�@�+@�ȴ@Ł@�  @�\)@�@��@�v�@°!@�-@�7L@���@���@�"�@�J@���@�%@��@�b@�l�@��R@�@��@��h@���@��j@�  @�;d@�ȴ@��+@��@��@�O�@��@�I�@��F@�l�@�;d@��H@�5?@�7L@���@���@���@���@�A�@���@�K�@�o@�n�@�$�@��@��#@�`B@���@� �@��@��
@��@�S�@�
=@��\@�{@���@�G�@��@���@��9@�z�@� �@��
@��F@���@���@�|�@�33@�v�@��#@�&�@�Ĝ@�Ĝ@��@��u@��D@�bN@��@��F@��@�K�@��@���@�~�@�ff@��@���@��-@�`B@�&�@��@��u@�r�@�1'@��@��P@�t�@�dZ@�S�@�o@��H@��+@�n�@�-@�{@��^@��@�/@�Q�@�9X@�9X@�A�@��F@�\)@�K�@�
=@���@��\@�ff@�^5@�V@�-@�{@�J@�@���@���@�X@�&�@�&�@��u@�(�@�(�@��@�"�@��@���@�5?@��7@��@��`@��u@��@�(�@���@�\)@�@���@���@���@���@���@��\@�5?@�J@�^5@���@�ȴ@���@��@�+@��@��H@��+@�E�@���@��@��^@�X@���@���@���@�Z@��w@���@�\)@�
=@��@��@���@���@�5?@���@�X@��@���@��`@�r�@���@���@��@�l�@�;d@�"�@�+@�
=@�ȴ@��\@�E�@�{@�@���@���@���@��h@�O�@��@��/@���@�1'@�1@���@��
@��w@�dZ@�oG�O�@�N<@~h
@w"�@pG@dr�@[|�@U\�@L(�@C��@=@@7�w@1��@*u@#�]@��@B�@ϫ@c @�@
�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111411111111111111111111;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oG�O�;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oB
T�B
T�B
T�B
T�B
S�B
S�B
R�B
R�B
R�B
Q�B
Q�B
P�B
Q�B
P�B
O�B
O�B
N�B
N�B
N�B
O�B
O�B
O�B
O�B
P�B
P�B
P�B
P�B
Q�B
YB
bNB
_;B
aHB
iyB
o�B
m�B
s�B
u�B
r�B
v�B
y�B
{�B
�B
�JB
�\B
�oB
��B
�'B
ÖB
ƨB
��B
�5B
�B
��B
��B
��BB
=B�B#�B'�B1'B;dBO�B`BBjB{�B�VB�{B�3B�}BǮB�B�TB�B��B�B-B@�BW
Bt�B��B��B�'B�3B�wB�hB�%B{�Bn�BO�BB�B5?BC�BM�B>wB$�BVB��B�5B��BǮB��B�XB�B��B|�Bn�B`BBN�BD�B?}BcTBe`B]/BT�B9XB"�B
�NB
��B
�dB
��B
��B
�B
m�B
]/B
N�B
A�B
+B
PB	�mB	�B	ÖB	��B	��B	�\B	�+B	y�B	o�B	ffB	YB	P�B	I�B	<jB	5?B	1'B	&�B	�B	�B	\B	%B��B��B��B��B�B�B��B�B�B�yB�B�B�yB�B�mB�fB�B�mB�`B�TB�HB�BB�;B�5B�/B�)B�B��B��B��B��BɺBĜB�qB�jB�RB�-B�B��B��B��B��B��B��B��B�{B�PB�PB�=B�7B�=B�1B�7B�%B�Bz�Bs�Bn�Bk�Bk�Bl�BjBl�Bk�Bm�Bn�Bs�Bs�Bv�Bw�Bx�B|�B}�By�Bp�Bm�Bq�Bp�Bq�Bs�By�By�By�Bu�Bx�B}�B{�B�B�B�B�B�B�B�B�B�%B�PB�\B�\B�VB�JB�=B�JB�bB�{B��B��B��B��B��B��B��B��B�B�B�B�9B�FB�RB��B��B��B��B��B��B��B��B��B��B��B�B�#B�5B�NB�fB�mB�sB�B�B�B�B�B�B�B��B��B��B	B	%B	1B		7B	
=B	VB	{B	�B	�B	�B	�B	�B	�B	!�B	!�B	&�B	-B	.B	.B	1'B	6FB	:^B	:^B	=qB	>wB	A�B	E�B	E�B	F�B	G�B	H�B	J�B	K�B	N�B	P�B	R�B	S�B	W
B	YB	YB	ZB	ZB	]/B	^5B	cTB	gmB	gmB	hsB	iyB	iyB	iyB	l�B	m�B	n�B	m�B	o�B	q�B	s�B	t�B	w�B	x�B	z�B	|�B	~�B	�B	�B	�B	�+B	�1B	�DB	�DB	�JB	�JB	�JB	�JB	�PB	�VB	�\B	�oB	�oB	�oB	�{B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�B	�B	�B	�B	�'B	�!B	�!B	�B	�!B	�-B	�3B	�3B	�9B	�9B	�FB	�XB	�XB	�dB	�}B	��B	ŢB	ƨB	ȴB	ȴB	��B	��B	��B	��B	��B	�
B	�B	�B	�/B	�BB	�HB	�TB	�TB	�ZB	�`B	�fB	�fB	�fB	�fB	�mB	�sB	�sB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
  B
  B
  B
  B
B
B
B
B
B
B
%B
%B
%B
+B
+B
+B
1B
	7B
�B
B
VB
*B
4�B
=�B
BB
D�B
I�B
P�B
YeB
]�B
fLB
k6B
o�B
t�B
y>B
{�B
~]B
��1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111411111111111111111111B
S�B
S�B
S�B
S�B
R�B
R�B
Q�B
Q�B
Q�B
P�B
P�B
O�B
P�B
O�B
NzB
N~B
MxB
MuB
MwB
NB
N~B
NyB
N~B
O�B
O�B
O~B
O�B
P�B
W�B
`�B
]�B
_�B
hB
n?B
l.B
rRB
tbB
qPB
uiB
xzB
z�B
��B
��B
��B
�B
�zB
��B
�5B
�CB
�vB
��B
�#B
�]B
�hB
�B�B�BEB"uB&�B/�B:BN}B^�BiBz�B��B�B��B�B�JBԡB��B�4B��B7B+�B?BU�BsXB�OB��B��B��B�B�B��Bz~Bm6BNxBA,B3�BB6BLsB=B#yB�B��B��B�lB�MB� B��B��B�#B{�Bm7B^�BMuBC9B> Ba�Bd B[�BS�B7�B!pB
��B
�,B
�B
��B
�>B
��B
l7B
[�B
MB
@.B
)�B
�B	�B	ԪB	�<B	��B	�/B	�B	��B	x�B	nHB	eB	W�B	O�B	HbB	;B	3�B	/�B	%�B	[B	/B	B	�B��B��B�B�oB�cB�`B�iB�TB�>B�#B�(B�0B�'B�0B�B�B�2B�B�B��B��B��B��B��B��B��B��BҥB̓B�~B̀B�hB�HB�!B�B��B��B��B��B�~B�dB�RB�cB�[B�5B�(B��B��B��B��B��B��B��B��B�By�BrcBmGBj5Bj6Bk<Bi,Bk8Bj3Bl=BmCBrdBreBuxBv~Bw�B{�B|�Bx�BoTBlCBpXBoTBpZBreBx�Bx�Bx�BtrBw�B|�Bz�B��B��B��B�B��B��B��B��B��B� B�B�	B�B��B��B��B�B�-B�CB�AB�ZB�sB�|B��B��B��B��B��B��B��B��B�B�2B�nB�}B�}B�mB�|B̂BΎBϓBНBҧB��B��B��B��B�B�B�$B�-B�4B�8B�7B�AB�SB�cB��B��B��B	 �B	�B	�B	�B	�B	B	,B	-B	5B	=B	;B	SB	iB	 zB	 |B	%�B	+�B	,�B	,�B	/�B	4�B	9	B	9B	<B	='B	@>B	DPB	DQB	EWB	F\B	GcB	IoB	JtB	M�B	O�B	Q�B	R�B	U�B	W�B	W�B	X�B	X�B	[�B	\�B	bB	f B	fB	g B	h)B	h(B	h&B	k=B	l<B	mEB	lBB	nJB	pZB	rdB	siB	v~B	w�B	y�B	{�B	}�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�
B	�B	�B	�B	�)B	�6B	�:B	�7B	�JB	�gB	�hB	�kB	�rB	�wB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�)B	�4B	�QB	�TB	�`B	�dB	́B	ϔB	ϓB	ЛB	ҤB	նB	��B	��B	��B	��B	��B	�B	�B	�	B	�B	�B	�B	�B	�B	�B	�B	�B	�.B	�6B	�8B	�AB	�CB	�JB	�KB	�JB	�KB	�PB	�XB	�YB	�cB	�dB	�bB	�pB	�uB	�~B	�|B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�G�O�B
�B
�B
B
(�B
3�B
<8B
@�B
CJB
HjB
O]B
XB
\aB
d�B
i�B
nLB
s5B
w�B
zGB
}B
�i1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111411111111111111111111<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
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
PRES            TEMP            PSAL            PRES_ADJUSTED = PRES - dP                                                                                                                                                                                                                                       none                                                                                                                                                                                                                                                            PSAL_ADJUSTED = PSAL + dS, where dS is calculated from a potential conductivity (ref to 0 dbar) multiplicative adjustment factor r.                                                                                                                             dP =0.15 dbar.                                                                                                                                                                                                                                                  none                                                                                                                                                                                                                                                            r =1(+/-0.0001), vertically averaged dS =-0.001(+/-0.002) in PSS-78.                                                                                                                                                                                            Pressures adjusted by using pressure offset at the sea surface. The quoted error is manufacturer specified accuracy in dbar.                                                                                                                                    The quoted error is manufacturer specified accuracy with respect to ITS-90 at time of laboratory calibration.                                                                                                                                                   Significant salinity drift detected. OWC least squares fit adopted. Map scales: x=6,3; y=2,1. Salinity also adjusted for effects of pressure adjustment. The quoted error is max[0.01, 1xOWC uncertainty] in PSS-78.                                            201906040940212019060409402120190604094021  AO  ARCAADJP                                                                    20181121041151    IP                G�O�G�O�G�O�                AO  ARGQQCPL                                                                    20181121041151  QCP$                G�O�G�O�G�O�5F03E           AO  ARGQQCPL                                                                    20181121041151  QCF$                G�O�G�O�G�O�0               UW  ARSQUWQC    WOD & nearby Argo as visual check                               20190604094021  IP                  G�O�G�O�G�O�                