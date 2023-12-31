CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       N2015-03-13T02:15:30Z AOML 3.0 creation; 2016-05-31T19:14:42Z UW 3.1 conversion     
references        (http://www.argodatamgt.org/Documentation   comment       	free text      user_manual_version       3.1    Conventions       Argo-3.1 CF-1.6    featureType       trajectoryProfile         @   	DATA_TYPE                  	long_name         	Data type      conventions       Argo reference table 1     
_FillValue                    6�   FORMAT_VERSION                 	long_name         File format version    
_FillValue                    6�   HANDBOOK_VERSION               	long_name         Data handbook version      
_FillValue                    6�   REFERENCE_DATE_TIME                 	long_name         !Date of reference for Julian days      conventions       YYYYMMDDHHMISS     
_FillValue                    6�   DATE_CREATION                   	long_name         Date of file creation      conventions       YYYYMMDDHHMISS     
_FillValue                    6�   DATE_UPDATE                 	long_name         Date of update of this file    conventions       YYYYMMDDHHMISS     
_FillValue                    7    PLATFORM_NUMBER                   	long_name         Float unique identifier    conventions       WMO float identifier : A9IIIII     
_FillValue                    7   PROJECT_NAME                  	long_name         Name of the project    
_FillValue                  @  7   PI_NAME                   	long_name         "Name of the principal investigator     
_FillValue                  @  7X   STATION_PARAMETERS           	            	long_name         ,List of available parameters for the station   conventions       Argo reference table 3     
_FillValue                  0  7�   CYCLE_NUMBER               	long_name         Float cycle number     conventions       =0...N, 0 : launch cycle (if exists), 1 : first complete cycle      
_FillValue         ��        7�   	DIRECTION                  	long_name         !Direction of the station profiles      conventions       -A: ascending profiles, D: descending profiles      
_FillValue                    7�   DATA_CENTRE                   	long_name         .Data centre in charge of float data processing     conventions       Argo reference table 4     
_FillValue                    7�   DC_REFERENCE                  	long_name         (Station unique identifier in data centre   conventions       Data centre convention     
_FillValue                     7�   DATA_STATE_INDICATOR                  	long_name         1Degree of processing the data have passed through      conventions       Argo reference table 6     
_FillValue                    7�   	DATA_MODE                  	long_name         Delayed mode or real time data     conventions       >R : real time; D : delayed mode; A : real time with adjustment     
_FillValue                    7�   PLATFORM_TYPE                     	long_name         Type of float      conventions       Argo reference table 23    
_FillValue                     7�   FLOAT_SERIAL_NO                   	long_name         Serial number of the float     
_FillValue                     8   FIRMWARE_VERSION                  	long_name         Instrument firmware version    
_FillValue                     8<   WMO_INST_TYPE                     	long_name         Coded instrument type      conventions       Argo reference table 8     
_FillValue                    8\   JULD               	long_name         ?Julian day (UTC) of the station relative to REFERENCE_DATE_TIME    standard_name         time   units         "days since 1950-01-01 00:00:00 UTC     conventions       8Relative julian days with decimal part (as parts of day)   
resolution        >Ey��0�:   
_FillValue        A.�~       axis      T           8`   JULD_QC                	long_name         Quality on date and time   conventions       Argo reference table 2     
_FillValue                    8h   JULD_LOCATION                  	long_name         @Julian day (UTC) of the location relative to REFERENCE_DATE_TIME   units         "days since 1950-01-01 00:00:00 UTC     conventions       8Relative julian days with decimal part (as parts of day)   
resolution        >Ey��0�:   
_FillValue        A.�~            8l   LATITUDE               	long_name         &Latitude of the station, best estimate     standard_name         latitude   units         degree_north   
_FillValue        @�i�       	valid_min         �V�        	valid_max         @V�        axis      Y           8t   	LONGITUDE                  	long_name         'Longitude of the station, best estimate    standard_name         	longitude      units         degree_east    
_FillValue        @�i�       	valid_min         �f�        	valid_max         @f�        axis      X           8|   POSITION_QC                	long_name         ,Quality on position (latitude and longitude)   conventions       Argo reference table 2     
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
resolution        :�o     �  U|   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                    ]p   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  _p   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                    gd   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         degree_Celsius     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  id   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  qX   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                    yL   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  {L   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                    �@   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  �@   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  0  �4   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                    �d   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                    �d   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                    �d   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  ,  �d   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                    ��   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                    ��   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                    ��   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                    ��   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  @  ��   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                    ��   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                    ��   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    ��   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar         �   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar         �   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�        �   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    �Argo profile    3.1 1.2 19500101000000  20150313021530  20160531121442  5903736 US ARGO PROJECT                                                 STEPHEN RISER                                                   PRES            TEMP            PSAL               jA   AO  4051_7090_106                   2C  D   APEX                            5368                            041511                          846 @�@�]��1   @�@픫?�@3�;dZ��dh�n��1   GPS     Primary sampling: mixed [deeper than nominal 985dbar: discrete; nominal 985dbar to surface: 2dbar-bin averaged]                                                                                                                                                    jA   A   A   @�ff@�  A   A   A@  A`  A�  A�  A�  A�  A�  A�  A�  A�  B   B  B  BffB   B(  B0  B8  B@  BH  BP  BX  B`  Bh  Bp  Bx  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  C   C  C  C  C  C
  C  C  C  C�C  C  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dts3Dy��D��D�VfD��fD�ٚD�fD�0 D��3D��fD�3D�6fD�i�D���D�3D�33D�l�D���D�3D�<�D�|�D��311111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   @���@�33@�33A��A=��A]��A}��A���A���A���A���A���A���A���A���BffBffB��BffB'ffB/ffB7ffB?ffBGffBOffBWffB_ffBgffBoffBwffBffB��3B��3B��3B��3B��3B��3B��3B��3B��3B��3B��3B��3B��3B��3B��3B��3Bó3Bǳ3B˳3Bϳ3Bӳ3B׳3B۳3B߳3B�3B�3B�3B�3B�3B��3B��3B��3CٚCٚCٚCٚC	ٚCٚCٚCٚC�4CٚCٚCٚCٚCٚCٚCٚC!ٚC#ٚC%ٚC'ٚC)ٚC+ٚC-ٚC/ٚC1ٚC3ٚC5ٚC7ٚC9ٚC;ٚC=ٚC?ٚCAٚCCٚCEٚCGٚCIٚCKٚCMٚCOٚCQٚCSٚCUٚCWٚCYٚC[ٚC]ٚC_ٚCaٚCcٚCeٚCgٚCiٚCkٚCmٚCoٚCqٚCsٚCuٚCwٚCyٚC{ٚC}ٚCٚC���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C�� C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���D vfD �fDvfD�fDvfD�fDvfD�fDvfD�fDvfD�fDvfD�fDvfD�fDvfD�fD	vfD	�fD
vfD
�fDvfD�fDvfD�fDvfD�fDvfD�fDvfD�fDvfD�fDvfD�fDvfD�fDvfD�fDvfD�fDvfD�fDvfD�fDvfD�fDvfD�fDvfD�fDvfD�fDvfD�fDvfD�fDvfD�fDvfD�fDvfD�fD vfD �fD!vfD!�fD"vfD"�fD#vfD#�fD$vfD$�fD%vfD%�fD&vfD&�fD'vfD'�fD(vfD(�fD)vfD)�fD*vfD*�fD+vfD+�fD,vfD,�fD-vfD-�fD.vfD.�fD/vfD/�fD0vfD0�fD1vfD1�fD2vfD2�fD3vfD3�fD4vfD4�fD5vfD5�fD6vfD6�fD7vfD7�fD8vfD8�fD9vfD9�fD:vfD:�fD;vfD;�fD<vfD<�fD=vfD=�fD>vfD>�fD?vfD?�fD@vfD@�fDAvfDA�fDBvfDB�fDCvfDC�fDDvfDD�fDEvfDE�fDFvfDF�fDGvfDG�fDHvfDH�fDIvfDI�fDJvfDJ�fDKvfDK�fDLvfDL�fDMvfDM�fDNvfDN�fDOvfDO�fDPvfDP�fDQvfDQ�fDRvfDR�fDSvfDS�fDTvfDT�fDUvfDU�fDVvfDV�fDWvfDW�fDXvfDX�fDYvfDY�fDZvfDZ�fD[vfD[�fD\vfD\�fD]vfD]�fD^vfD^�fD_vfD_�fD`vfD`�fDavfDa�fDbvfDb�fDcvfDc�fDdvfDd�fDevfDe�fDfvfDf�fDgvfDg�fDhvfDh�fDivfDi�fDjvfDj�fDkvfDk�fDlvfDl�fDmvfDm�fDnvfDn�fDovfDo�fDpvfDp�fDqvfDq�fDrvfDr�fDsvfDs�fDti�Dy�3D� D�Q�D���D���D��D�+3D��fD���D�fD�1�D�d�D�� D��fD�.fD�h D�� D��fD�8 D�x D��f11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   @��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��A��A���A���A���A��TA��`A��`A��yA��mA��yA��A��A��A��A��A��A��mA��yA��`A��/A+A�I�A�v�A�A���A�?}A���A��!A�`BA�oA��`A��#A���A���A��jA���A��\A�l�A�\)A�"�A�A��A��A���A�C�A��A�dZA���A�VA��A��/A��+A��#A�&�A��A��A�C�A���A�A�5?A�VA���A��DA�+A���A���A�&�A���A��A�?}A��A��/A���A��A��HA��wA��A���A���A��A�?}A�A��\A�&�A��;A��7A��A�+A��#A�p�A�S�A��/A�v�A���A��FA�`BA�bA��DA��hA�bNA�ƨA�O�A���A�n�A��AoA}l�A|Az��Ax(�Aw��Av  At5?Aq?}An-Aj��Ai��Ah��Af�yAc�Aa��A`�jA_��A^E�A\�uAZAXffAWS�ATr�APA�AOhsAN�`AM�wAL(�AI��AH��AGXADv�AB1'AA�AA&�A@�HA@ĜA@�uA?A?
=A>  A<��A<$�A;��A;VA:VA9�mA9/A7��A5%A2�A1�mA1t�A0�A0�A/x�A/33A-O�A+�hA*�RA(^5A( �A'x�A'%A$�!A#��A#��A"�DA!�
A!dZA =qA�RA��AA��A��A��A
=A �A�A�FA1A��A�7A�!A��A
��A
jA
�A	hsA�`A�A�A�\A��AffA�7A1A`BAz�A+A �HA ȴA �A 1'@���@���@��R@�{@�@��@�X@�Ĝ@�|�@�=q@��9@��@�-@��@��;@@�@���@�  @�@���@�1'@�\@��@�?}@�w@���@�-@��@�/@�r�@��@߮@�E�@��/@�  @�\)@�v�@٩�@�7L@��`@��;@Ցh@���@��@д9@�hs@ʸR@���@�7L@�&�@��@�A�@�=q@Ų-@�?}@ēu@�(�@���@�C�@�-@��^@��@��D@���@��D@�(�@�\)@�ȴ@�ff@��@���@�hs@�Ĝ@�1@��@�ff@��@���@�G�@��@�  @�bN@��w@�;d@��@��+@�ff@�-@��@���@�Ĝ@�Q�@�1'@��@���@�C�@�"�@���@�M�@��@�x�@�X@��`@��P@�ȴ@��R@��R@���@���@�$�@���@��@�/@��@���@���@��@���@��w@���@�+@���@��+@�5?@�@���@��7@�X@�?}@���@��/@��@�r�@���@���@�/@�?}@��@�Ĝ@��u@�z�@�r�@�Z@�1'@�  @��;@��w@���@��@�l�@�C�@�C�@��y@�E�@��/@�(�@�1@�b@�b@���@�t�@�S�@���@��F@�dZ@�
=@���@�v�@�V@�5?@���@���@�?}@��D@�1@��;@��m@�  @���@��m@��@���@�n�@�@���@�O�@���@���@��D@�j@�A�@�t�@�@��@���@���@�V@��@���@�`B@���@�bN@�Q�@�1@��P@�l�@�\)@�+@�@��y@�ȴ@��!@�ff@�^5@�=q@��T@��@�`B@��@���@��@�A�@��w@��@�t�@�33@��y@���@�^5@�J@��T@���@���@�p�@�O�@��@���@��j@��@��P@��@�ff@�-@�~�@�v�@�M�@�-@�@���@���@��@��D@�j@�I�@�1'@��@��m@��w@���@���@�|�@�K�@�"�@�@��@��+@�n�@�-@��@�x�@�7L@�V@���@��@�bN@� �@��@���@y7L@nȴ@c"�@[C�@Q��@JM�@D1@?�P@9�@3��@.v�@'�@#�m@K�@ƨ@�w@ƨ@��@9X11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   A��A���A���A���A��TA��`A��`A��yA��mA��yA��A��A��A��A��A��A��mA��yA��`A��/A+A�I�A�v�A�A���A�?}A���A��!A�`BA�oA��`A��#A���A���A��jA���A��\A�l�A�\)A�"�A�A��A��A���A�C�A��A�dZA���A�VA��A��/A��+A��#A�&�A��A��A�C�A���A�A�5?A�VA���A��DA�+A���A���A�&�A���A��A�?}A��A��/A���A��A��HA��wA��A���A���A��A�?}A�A��\A�&�A��;A��7A��A�+A��#A�p�A�S�A��/A�v�A���A��FA�`BA�bA��DA��hA�bNA�ƨA�O�A���A�n�A��AoA}l�A|Az��Ax(�Aw��Av  At5?Aq?}An-Aj��Ai��Ah��Af�yAc�Aa��A`�jA_��A^E�A\�uAZAXffAWS�ATr�APA�AOhsAN�`AM�wAL(�AI��AH��AGXADv�AB1'AA�AA&�A@�HA@ĜA@�uA?A?
=A>  A<��A<$�A;��A;VA:VA9�mA9/A7��A5%A2�A1�mA1t�A0�A0�A/x�A/33A-O�A+�hA*�RA(^5A( �A'x�A'%A$�!A#��A#��A"�DA!�
A!dZA =qA�RA��AA��A��A��A
=A �A�A�FA1A��A�7A�!A��A
��A
jA
�A	hsA�`A�A�A�\A��AffA�7A1A`BAz�A+A �HA ȴA �A 1'@���@���@��R@�{@�@��@�X@�Ĝ@�|�@�=q@��9@��@�-@��@��;@@�@���@�  @�@���@�1'@�\@��@�?}@�w@���@�-@��@�/@�r�@��@߮@�E�@��/@�  @�\)@�v�@٩�@�7L@��`@��;@Ցh@���@��@д9@�hs@ʸR@���@�7L@�&�@��@�A�@�=q@Ų-@�?}@ēu@�(�@���@�C�@�-@��^@��@��D@���@��D@�(�@�\)@�ȴ@�ff@��@���@�hs@�Ĝ@�1@��@�ff@��@���@�G�@��@�  @�bN@��w@�;d@��@��+@�ff@�-@��@���@�Ĝ@�Q�@�1'@��@���@�C�@�"�@���@�M�@��@�x�@�X@��`@��P@�ȴ@��R@��R@���@���@�$�@���@��@�/@��@���@���@��@���@��w@���@�+@���@��+@�5?@�@���@��7@�X@�?}@���@��/@��@�r�@���@���@�/@�?}@��@�Ĝ@��u@�z�@�r�@�Z@�1'@�  @��;@��w@���@��@�l�@�C�@�C�@��y@�E�@��/@�(�@�1@�b@�b@���@�t�@�S�@���@��F@�dZ@�
=@���@�v�@�V@�5?@���@���@�?}@��D@�1@��;@��m@�  @���@��m@��@���@�n�@�@���@�O�@���@���@��D@�j@�A�@�t�@�@��@���@���@�V@��@���@�`B@���@�bN@�Q�@�1@��P@�l�@�\)@�+@�@��y@�ȴ@��!@�ff@�^5@�=q@��T@��@�`B@��@���@��@�A�@��w@��@�t�@�33@��y@���@�^5@�J@��T@���@���@�p�@�O�@��@���@��j@��@��P@��@�ff@�-@�~�@�v�@�M�@�-@�@���@���@��@��D@�j@�I�@�1'@��@��m@��w@���@���@�|�@�K�@�"�@�@��@��+@�n�@�-@��@�x�@�7L@�V@���@��@�bN@� �@��@���@y7L@nȴ@c"�@[C�@Q��@JM�@D1@?�P@9�@3��@.v�@'�@#�m@K�@ƨ@�w@ƨ@��@9X11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   ;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oB.B/B/B/B.B.B.B.B.B/B/B/B0!B/B/B/B0!B0!B0!B/BA�B��B�BN�B]/BgmBr�B~�B�7B�bB�uB�{B��B��B��B��B�{B�{B�{B�uB�uB�uB�uB�hB�PB�\B�DB� Bz�By�Bu�Bo�Bq�BaHBR�BD�B9XB33B�B  B�TB�B��BɺBÖB�RB��B��B�oB�\B�VB�DB�1B�Br�BbNBK�B,B�BJB��B�fB��B��BĜB�B��B�+B~�BhsBH�B0!B&�B�BDBB
��B
��B
�fB
��B
�jB
��B
�hB
�DB
�B
{�B
o�B
e`B
ZB
H�B
B�B
6FB
%�B
bB	��B	�TB	�B	��B	��B	�B	��B	��B	�hB	�1B	{�B	m�B	dZB	[#B	J�B	6FB	/B	+B	$�B	�B	hB	DB	B��B�B�B�B�B�B�B�yB�fB�TB�HB�;B�5B�)B�#B�B��B��B�}B�XB�?B�-B�B�B�B��B��B��B��B��B��B��B�{B�hB�\B�VB�PB�DB�=B�1B�%B�B� B~�B|�B{�B}�B|�Bz�Bv�Bt�Bs�Br�Bo�Bk�BiyBiyBhsBgmBhsBhsBhsBgmBffBiyBjBm�Bn�Bo�Br�Bs�Bs�Bs�Br�Bq�Bp�Bu�Bx�Bx�Bx�Bx�Bx�By�By�Bx�Bw�Bw�Bw�Bx�Bz�Bz�Bz�B~�B�B�1B�DB�PB�PB�PB�PB�bB�oB�{B�{B��B��B��B��B��B��B��B�B�B�B�B�B��B��B��B�'B�B�B�B�-B�-B�'B�9B�jB�wB��BBÖBȴBɺB��B��B�
B�B�B�5B�sB�B�B�B�B��B��B��B��B��B��B	  B	B	B	B	B	DB	hB	{B	�B	�B	�B	�B	�B	�B	#�B	&�B	&�B	(�B	)�B	-B	-B	1'B	33B	5?B	8RB	9XB	;dB	D�B	I�B	I�B	I�B	I�B	J�B	L�B	O�B	Q�B	S�B	T�B	W
B	XB	ZB	`BB	bNB	bNB	dZB	hsB	hsB	jB	k�B	k�B	l�B	l�B	m�B	q�B	t�B	u�B	y�B	|�B	�B	�7B	�DB	�PB	�bB	�oB	�uB	�uB	�{B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�-B	�3B	�?B	�LB	�XB	�XB	�XB	�dB	�qB	�qB	�jB	�qB	�}B	��B	B	ÖB	ÖB	ĜB	ŢB	ƨB	ŢB	ƨB	ȴB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�
B	�B	�B	�#B	�5B	�/B	�5B	�BB	�HB	�HB	�HB	�HB	�HB	�NB	�NB	�NB	�NB	�NB	�TB	�ZB	�ZB	�mB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B
  B
  B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
%B
%B
%B
+B
+B
+B
+B
1B
	7B
	7B
	7B

=B

=B

=B
JB
PB
�B
!�B
(�B
1'B
7LB
>wB
D�B
H�B
L�B
R�B
XB
\)B
bNB
dZB
iyB
k�B
o�B
r�B
u�B
z�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   B.#B/)B/)B/)B.!B.!B.#B.#B.#B/&B/$B/&B0/B/&B/&B/&B0+B01B01B/&BA�B��B�BN�B]ABg|Br�BB�IB�sB��B��B��B��B��B��B��B��B��B��B��B��B��B�vB�dB�nB�YB�Bz�By�Bu�Bo�Bq�BaXBSBD�B9jB3CB�B B�dB�B��B��BåB�cB�B��B�~B�lB�dB�PB�>B�Br�Bb[BK�B,B�BTB��B�uB�B��BĬB�'B��B�;BBh�BH�B00B&�B�BSB+B
�B
��B
�vB
�B
�{B
��B
�{B
�XB
�1B
{�B
o�B
euB
Z1B
H�B
B�B
6]B
%�B
xB	��B	�mB	�-B	��B	��B	�"B	��B	��B	��B	�NB	|B	m�B	dwB	[AB	J�B	6dB	/;B	+#B	$�B	�B	�B	dB	=B�
B��B��B��B�B��B�B�B�B�vB�mB�]B�ZB�LB�EB�3B�B��B��B�{B�eB�RB�@B�3B�'B�B�B��B��B��B��B��B��B��B��B�{B�vB�lB�dB�YB�LB�GB�(B#B}B|B~B}B{	Bv�Bt�Bs�Br�Bo�Bk�Bi�Bi�Bh�Bg�Bh�Bh�Bh�Bg�Bf�Bi�Bj�Bm�Bn�Bo�Br�Bs�Bs�Bs�Br�Bq�Bp�Bu�Bx�Bx�Bx�Bx�Bx�BzBzBx�Bw�Bw�Bw�Bx�B{
B{B{
B!B�:B�XB�lB�wB�xB�yB�yB��B��B��B��B��B��B��B��B��B�
B�B�)B�4B�4B�,B�(B�B��B��B�MB�CB�;B�DB�SB�SB�MB�^B��B��B��B´BùB��B��B��B�B�/B�5B�;B�YB�B�B�B��B��B��B��B��B��B��B��B	 #B	4B	:B	=B	<B	dB	�B	�B	�B	�B	�B	�B	�B	�B	#�B	'	B	'
B	)B	*B	--B	--B	1GB	3SB	5aB	8qB	9xB	;�B	D�B	I�B	I�B	I�B	I�B	J�B	L�B	O�B	RB	TB	UB	W*B	X1B	Z<B	`aB	blB	blB	dyB	h�B	h�B	j�B	k�B	k�B	l�B	l�B	m�B	q�B	t�B	u�B	y�B	}B	�2B	�WB	�aB	�mB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�-B	�7B	�KB	�PB	�[B	�gB	�sB	�sB	�sB	��B	��B	��B	��B	��B	��B	��B	ªB	òB	òB	ĶB	ſB	��B	żB	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�B	�B	�$B	�)B	�4B	�?B	�RB	�IB	�SB	�\B	�bB	�eB	�bB	�fB	�cB	�hB	�hB	�iB	�jB	�hB	�qB	�tB	�uB	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	�
B	�B
 B
 B
 B
 B
!B
B
 B
 B
 B
B
B
B
%B
&B
,B
:B
=B
@B
<B
DB
EB
FB
EB
LB
	QB
	RB
	PB

TB

UB

YB
eB
jB
�B
!�B
)B
1AB
7dB
>�B
D�B
H�B
L�B
SB
X(B
\@B
bfB
dqB
i�B
k�B
o�B
r�B
u�B
z�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   <#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
PRES            TEMP            PSAL            PRES_ADJUSTED = PRES - dP                                                                                                                                                                                                                                       none                                                                                                                                                                                                                                                            PSAL_ADJUSTED = sw_salt( sw_cndr(PSAL,TEMP,PRES), TEMP, PRES_ADJUSTED )                                                                                                                                                                                         dP =0.15 dbar.                                                                                                                                                                                                                                                  none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            Pressures adjusted by using pressure offset at the sea surface. The quoted error is manufacturer specified accuracy in dbar.                                                                                                                                    The quoted error is manufacturer specified accuracy with respect to ITS-90 at time of laboratory calibration.                                                                                                                                                   No significant salinity drift detected. Salinity adjusted for effects of pressure adjustment. The quoted error is max[0.01, 1xOW uncertainty] in PSS-78.                                                                                                        201605311214422016053112144220160531121442  AO  ARCAADJP                                                                    20150313021530    IP                G�O�G�O�G�O�                AO  ARGQQCPL                                                                    20150313021530  QCP$                G�O�G�O�G�O�DFB5E           AO  ARGQQCPL                                                                    20150313021530  QCF$                G�O�G�O�G�O�0               UW  ARSQUWQC    WOD & nearby Argo as visual check                               20160531121442  IP                  G�O�G�O�G�O�                