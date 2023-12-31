CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       N2015-06-27T19:18:12Z AOML 3.0 creation; 2016-08-07T21:17:38Z UW 3.1 conversion     
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
_FillValue                 �  A|   PRES_ADJUSTED            
      	   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  Ct   PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  KP   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         decibar    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  MH   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  U$   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ]    TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  ^�   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  f�   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         degree_Celsius     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  h�   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  p�   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  x�   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  z|   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �X   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  �P   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  0  �,   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                    �\   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                    �\   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                    �\   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  ,  �\   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                    ��   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                    ��   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                    ��   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                    ��   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  @  ��   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                    ��   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                    ��   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    ��   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar         ��   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar         �    HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�        �   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    �Argo profile    3.1 1.2 19500101000000  20150627191812  20160807141738  5904460 US ARGO PROJECT                                                 STEPHEN RISER                                                   PRES            TEMP            PSAL               :A   AO  5285_8895_058                   2C  D   APEX                            6487                            072314                          846 @�[��� 1   @�[�q�@,�j~��#�c�S���1   GPS     Primary sampling: mixed [deeper than nominal 985dbar: discrete; nominal 985dbar to surface: 2dbar-bin averaged]                                                                                                                                                    :A   B   B   @�33@�  A   A   A>ffA`  A~ffA�  A�  A�  A�  A�  A�  A�  B   B  B  B  B   B(  B0  B8  B@  BHffBPffBX  Ba��Bg��Bp  Bx  B�  B�33B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�33B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  C   C  C  C  C  C
  C  C  C  C�C  C  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>�C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX�CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-y�D-��D.� D/fD/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dt� Dt�fDy� D��D�L�D��fD���D�  D�&fD�l�D��fD��3D�Y�D�|�DǠ D�ٚ11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 @�ff@�33A��A%��AD  Ae��A�  A���A���A���A���A���A���A���BffB	ffBffBffB!ffB)ffB1ffB9ffBAffBI��BQ��BYffBc  Bi  BqffByffB��3B��fB��3B��3B��3B��3B��3B��3B��3B��3B��3B��3B��3B��3B��3B��fB��3Bĳ3Bȳ3B̳3Bг3BԳ3Bس3Bܳ3B�3B�3B�3B�3B�3B��3B��3B��3C Y�CY�CY�CY�CY�C
Y�CY�CY�CY�Cs4CY�CY�CY�CY�CY�CY�C Y�C"Y�C$Y�C&Y�C(Y�C*Y�C,Y�C.Y�C0Y�C2Y�C4Y�C6Y�C8Y�C:Y�C<Y�C>s4C@Y�CBY�CDY�CFY�CHY�CJY�CLY�CNY�CPY�CRY�CTY�CVY�CXs4CZY�C\Y�C^Y�C`Y�CbY�CdY�CfY�ChY�CjY�ClY�CnY�CpY�CrY�CtY�CvY�CxY�CzY�C|Y�C~Y�C�,�C�,�C�,�C�,�C�,�C�,�C�,�C�,�C�,�C�,�C�,�C�,�C�,�C�,�C�,�C�,�C�,�C�,�C�,�C�,�C�,�C�,�C�,�C�,�C�,�C�,�C�,�C�,�C�,�C�,�C�,�C�,�C�,�C�,�C�,�C�,�C�,�C�,�C�,�C�,�C�,�C�,�C�,�C�,�C�,�C�,�C�,�C�,�C�,�C�,�C�,�C�,�C�,�C�,�C�,�C�,�C�,�C�,�C�,�C�,�C�,�C�,�C�,�C�,�C�,�C�,�C�,�C�,�C�,�C�,�C�,�C�,�C�,�C�,�C�,�C�,�C�,�C�,�C�,�C�,�C�,�C�,�C�,�C�,�C�,�C�,�C�,�C�,�C�,�C�,�C�,�C�,�C�,�C�,�C�,�C�,�C�,�C�,�C�,�C�,�C�,�C�,�C�,�C�,�C�,�C�,�C�,�C�,�C�,�C�,�C�,�C�,�C�,�C�,�C�,�C�,�C�,�C�,�C�,�C�,�C�,�C�,�C�,�C�,�C�,�C�,�C�,�C�,�D fD �fDfD�fDfD�fDfD�fDfD�fDfD�fDfD�fDfD�fDfD�fD	fD	�fD
fD
�fDfD�fDfD�fDfD�fDfD�fDfD�fDfD�fDfD�fDfD�fDfD�fDfD�fDfD�fDfD�fDfD�fDfD�fDfD�fDfD�fDfD�fDfD�fDfD�fDfD�fDfD�fD fD �fD!fD!�fD"fD"�fD#fD#�fD$fD$�fD%fD%�fD&fD&�fD'fD'�fD(fD(�fD)fD)�fD*fD*�fD+fD+�fD,fD,�fD-fD-� D. D.�fD/�D/�fD0fD0�fD1fD1�fD2fD2�fD3fD3�fD4fD4�fD5fD5�fD6fD6�fD7fD7�fD8fD8�fD9fD9�fD:fD:�fD;fD;�fD<fD<�fD=fD=�fD>fD>�fD?fD?�fD@fD@�fDAfDA�fDBfDB�fDCfDC�fDDfDD�fDEfDE�fDFfDF�fDGfDG�fDHfDH�fDIfDI�fDJfDJ�fDKfDK�fDLfDL�fDMfDM�fDNfDN�fDOfDO�fDPfDP�fDQfDQ�fDRfDR�fDSfDS�fDTfDT�fDUfDU�fDVfDV�fDWfDW�fDXfDX�fDYfDY�fDZfDZ�fD[fD[�fD\fD\�fD]fD]�fD^fD^�fD_fD_�fD`fD`�fDafDa�fDbfDb�fDcfDc�fDdfDd�fDefDe�fDffDf�fDgfDg�fDhfDh�fDifDi�fDjfDj�fDkfDk�fDlfDl�fDmfDm�fDnfDn�fDofDo�fDpfDp�fDqfDq�fDrfDr�fDsfDs�fDtfDt�fDt��Dy�fD� D�X D���D�� D�3D�1�D�x D���D��fD�d�D�� Dǫ3D���11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 @��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��A��A��A��A��A��A��A��A���A���A���A���A���A���A���A���A���A���A���A�  A�A�  A���A�  A���A���A�  AՍPAԟ�A���A��yA�Q�A���A�1'AľwA�r�A�C�A��A��HA�XA�VA�p�A���A��^A� �A��A��PA�v�A��A���A���A�E�A�(�A��mA��mA���A��7A�oA�
=A�XA��RA���A�VA�-A�\)A�ffA��-A��!A��A��A��RA��A�E�A���A��A�C�A�dZA�A�S�A���A��A�JA���A�G�A��9A�  A��DA�;dA~bAzA�Av�As`BAi?}Af��Ad�jAZ�RAS�TAQ7LAOVAM�AJ-AE��AB~�A@  A<��A;�A9�A8�9A8bNA6jA4��A3K�A2��A2��A1dZA0��A0��A1p�A0�A-�#A,ȴA, �A+�wA+��A+\)A*�yA*�DA*  A)�^A)��A)7LA)7LA)?}A(�HA(ZA(bA'��A&��A&��A&I�A%�TA%�PA$��A$��A$bNA#�A"ȴA"A�A!�TA!�hA!33A ��A ��A A�A 5?A bA��A"�A��AjA�A  A��A\)A�A��AbNAJA�7A�/AȴA�9A�\AbNA1A��A��A�uAbA;dA�DA�A�#A�
A��A�wA��AO�A��A-A�A�hA&�A�A�A`BA"�A�A��AK�AAffA��A?}A�A��A �A�7A+A
ĜA
Q�A
=qA
A	�mA	��A	
=A�+A1A��At�A?}A��A�A��A�uA~�AZA��A�Az�AI�A5?A�TA�FAXA%AA�jAVA1A��A��A��Ap�A�A ��A ��A ��A �DA j@���@���@��u@�|�@��@��R@�E�@�@�7L@��@�Z@�b@�dZ@���@�v�@�n�@�=q@��@�X@�ƨ@�^5@�^@�`B@�G�@�Ĝ@�1'@�dZ@@�n�@���@�7L@�@� �@�o@ꟾ@��@���@��@��;@��H@�M�@��T@��@�@��
@�V@��#@�V@�A�@ߍP@�
=@ޏ\@��@�?}@�1@�|�@ڗ�@�@��@���@�hs@���@ش9@ؓu@��@�|�@��@���@֏\@�{@��@���@�r�@���@�K�@�n�@���@щ7@��@�9X@�\)@�ff@��#@�p�@�`B@�O�@���@�Q�@���@�C�@ʰ!@ʏ\@�n�@�-@ɲ-@�X@�V@��/@ȼj@ț�@�r�@�9X@���@�+@���@ư!@�E�@���@š�@�?}@���@Ĭ@ě�@�bN@�9X@�Z@��@�l�@�o@�@�=q@��@��@��-@�X@���@���@� �@��w@�+@�{@���@�O�@��@��`@��@���@�+@���@���@���@�-@���@��-@�p�@�&�@���@�Z@���@�33@��H@���@�x�@��@�9X@���@�"�@��\@��T@�7L@�%@��@�I�@��@��@�o@���@�E�@��-@�V@���@�Ĝ@��D@��@��;@��P@�+@��H@���@�v�@�-@��-@�x�@�hs@��7@�?}@��@��j@�r�@�1'@��w@�C�@�o@��@�@��7@�G�@�V@���@�r�@�Z@�Q�@��@�+@��@�ȴ@��!@��\@�$�@�7L@��@��j@���@�I�@��@�ƨ@�C�@��y@��+@��#@��@��@���@��@�Q�@��
@��P@�33@��y@���@�^5@�-@��-@���@�/@���@���@��m@�\)@��R@���@��@���@���@z��@m��@d(�@^��@X�9@Q�@H�`@@�u@7�P@0bN11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111411111111111111 A��A��A��A��A��A��A��A���A���A���A���A���A���A���A���A���A���A���A�  A�A�  A���A�  A���A���A�  AՍPAԟ�A���A��yA�Q�A���A�1'AľwA�r�A�C�A��A��HA�XA�VA�p�A���A��^A� �A��A��PA�v�A��A���A���A�E�A�(�A��mA��mA���A��7A�oA�
=A�XA��RA���A�VA�-A�\)A�ffA��-A��!A��A��A��RA��A�E�A���A��A�C�A�dZA�A�S�A���A��A�JA���A�G�A��9A�  A��DA�;dA~bAzA�Av�As`BAi?}Af��Ad�jAZ�RAS�TAQ7LAOVAM�AJ-AE��AB~�A@  A<��A;�A9�A8�9A8bNA6jA4��A3K�A2��A2��A1dZA0��A0��A1p�A0�A-�#A,ȴA, �A+�wA+��A+\)A*�yA*�DA*  A)�^A)��A)7LA)7LA)?}A(�HA(ZA(bA'��A&��A&��A&I�A%�TA%�PA$��A$��A$bNA#�A"ȴA"A�A!�TA!�hA!33A ��A ��A A�A 5?A bA��A"�A��AjA�A  A��A\)A�A��AbNAJA�7A�/AȴA�9A�\AbNA1A��A��A�uAbA;dA�DA�A�#A�
A��A�wA��AO�A��A-A�A�hA&�A�A�A`BA"�A�A��AK�AAffA��A?}A�A��A �A�7A+A
ĜA
Q�A
=qA
A	�mA	��A	
=A�+A1A��At�A?}A��A�A��A�uA~�AZA��A�Az�AI�A5?A�TA�FAXA%AA�jAVA1A��A��A��Ap�A�A ��A ��A ��A �DA j@���@���@��u@�|�@��@��R@�E�@�@�7L@��@�Z@�b@�dZ@���@�v�@�n�@�=q@��@�X@�ƨ@�^5@�^@�`B@�G�@�Ĝ@�1'@�dZ@@�n�@���@�7L@�@� �@�o@ꟾ@��@���@��@��;@��H@�M�@��T@��@�@��
@�V@��#@�V@�A�@ߍP@�
=@ޏ\@��@�?}@�1@�|�@ڗ�@�@��@���@�hs@���@ش9@ؓu@��@�|�@��@���@֏\@�{@��@���@�r�@���@�K�@�n�@���@щ7@��@�9X@�\)@�ff@��#@�p�@�`B@�O�@���@�Q�@���@�C�@ʰ!@ʏ\@�n�@�-@ɲ-@�X@�V@��/@ȼj@ț�@�r�@�9X@���@�+@���@ư!@�E�@���@š�@�?}@���@Ĭ@ě�@�bN@�9X@�Z@��@�l�@�o@�@�=q@��@��@��-@�X@���@���@� �@��w@�+@�{@���@�O�@��@��`@��@���@�+@���@���@���@�-@���@��-@�p�@�&�@���@�Z@���@�33@��H@���@�x�@��@�9X@���@�"�@��\@��T@�7L@�%@��@�I�@��@��@�o@���@�E�@��-@�V@���@�Ĝ@��D@��@��;@��P@�+@��H@���@�v�@�-@��-@�x�@�hs@��7@�?}@��@��j@�r�@�1'@��w@�C�@�o@��@�@��7@�G�@�V@���@�r�@�Z@�Q�@��@�+@��@�ȴ@��!@��\@�$�@�7L@��@��j@���@�I�@��@�ƨ@�C�@��y@��+@��#@��@��@���@��@�Q�@��
@��P@�33@��y@���@�^5@�-@��-@���@�/@���@���@��m@�\)G�O�@���@��@���@���@z��@m��@d(�@^��@X�9@Q�@H�`@@�u@7�P@0bN11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111411111111111111 ;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oG�O�;o;o;o;o;o;o;o;o;o;o;o;o;o;oB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	ÖB	�BB�
B;dBP�BZBcTB}�B��B��B�}B�B�ZB�B��BBBPB"�B(�B0!B7LB8RB;dB;dB6FB,B)�B#�B{BDB  B��B��B�yB�BB�BƨB��B�BbNBQ�BO�BF�B:^B.B%B�`B�}B|�B_;BS�B8RB!�B
=B
��B
ɺB
��B
\)B
@�B
 �B
B	�B	��B	�-B	w�B	dZB	O�B	�B	B��B��B��B�B�TB�NB��B	�B	'�B	6FB	A�B	B�B	T�B	o�B	�=B	�B	ǮB	��B	ȴB	�fB
+B
  B	��B	��B	��B
B
oB
oB
oB
{B
�B
�B
�B
�B
"�B
(�B
'�B
&�B
)�B
,B
+B
.B
1'B
0!B
2-B
5?B
8RB
9XB
9XB
:^B
<jB
=qB
=qB
<jB
=qB
>wB
>wB
>wB
>wB
?}B
B�B
C�B
E�B
F�B
H�B
H�B
G�B
F�B
F�B
H�B
G�B
H�B
L�B
L�B
L�B
L�B
K�B
K�B
I�B
H�B
G�B
G�B
F�B
F�B
E�B
D�B
D�B
D�B
D�B
C�B
B�B
A�B
@�B
@�B
?}B
=qB
=qB
<jB
<jB
;dB
:^B
:^B
:^B
9XB
7LB
8RB
8RB
7LB
6FB
5?B
5?B
49B
33B
2-B
2-B
1'B
0!B
0!B
0!B
/B
/B
0!B
2-B
33B
33B
49B
49B
49B
33B
33B
1'B
.B
.B
0!B
0!B
2-B
2-B
2-B
2-B
5?B
5?B
5?B
33B
49B
49B
49B
33B
33B
33B
2-B
2-B
2-B
1'B
/B
-B
,B
,B
,B
-B
-B
,B
,B
+B
)�B
(�B
'�B
&�B
'�B
'�B
&�B
%�B
$�B
!�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
uB
bB
\B
VB
JB
DB

=B

=B

=B
	7B
	7B
	7B
1B
	7B
	7B
	7B
1B
+B
+B
+B
%B
B
B
B
B
B
  B
  B	��B	��B	��B	��B	��B	��B	��B
  B
B
  B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
  B
  B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
%B
B
%B
+B
1B
1B
1B
1B
	7B
1B
1B
1B
1B
1B
	7B

=B
	7B
	7B

=B

=B

=B

=B

=B

=B

=B

=B

=B
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

=B

=B

=B

=B

=B

=B

=B
DB
DB
DB
DB
DB
JB
\B
\B
\B
\B
\B
VB
VB
PB
PB
PB
PB
PB
PB
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
hB
hB
hB
hB
oB
oB
oB
oB
uB
oB
uB
uB
uB
uB
uB
uB
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
)�B
"�B
%�B
+B
49B
8RB
=qB
D�B
H�B
K�B
O�B
W
B
\)B
`BB
e`11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111411111111111111 B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	ÂB	�-B��B;GBP�BZ Bc;B}�B��B��B�aB��B�AB�lB��B�B�B6B"�B(�B0B74B8:B;JB;IB6.B+�B)�B#�B]B$B��B��B��B�^B�&B��BƉB��B��Bb.BQ�BO�BF�B:;B-�BB�AB�]B|�B_BS�B83B!�B
B
��B
ɛB
�fB
\B
@dB
 �B
�B	�cB	��B	�B	w�B	dAB	O�B	�B	B��B��B��B�qB�?B�:B��B	uB	'�B	6.B	AoB	BwB	T�B	o�B	�"B	��B	ǏB	ʢB	ȕB	�EB

B	��B	��B	��B	��B
�B
NB
NB
OB
WB
kB
�B
�B
�B
"�B
(�B
'�B
&�B
)�B
+�B
*�B
-�B
1B
/�B
2B
5B
80B
93B
95B
:;B
<EB
=MB
=OB
<FB
=KB
>SB
>SB
>UB
>VB
?WB
BkB
CqB
E}B
F�B
H�B
H�B
G�B
F�B
F�B
H�B
G�B
H�B
L�B
L�B
L�B
L�B
K�B
K�B
I�B
H�B
G�B
G�B
F�B
F�B
EB
DwB
DwB
DwB
DwB
CqB
BmB
AcB
@_B
@_B
?XB
=JB
=LB
<GB
<FB
;@B
::B
:;B
:9B
93B
7'B
8.B
8.B
7'B
6#B
5B
5B
4B
3B
2	B
2	B
1B
/�B
/�B
/�B
.�B
.�B
/�B
2	B
3B
3B
4B
4B
4B
3B
3B
1B
-�B
-�B
/�B
/�B
2
B
2
B
2B
2B
5B
5B
5B
3B
4B
4B
4B
3B
3B
3B
2B
2B
2B
1B
.�B
,�B
+�B
+�B
+�B
,�B
,�B
+�B
+�B
*�B
)�B
(�B
'�B
&�B
'�B
'�B
&�B
%�B
$�B
!�B
�B
�B
�B
�B
zB
yB
nB
oB
oB
bB
ZB
\B
PB
<B
8B
1B
$B
B

B

B

B
	B
	B
	B
B
	B
	B
	B
B
B
B
B
 B
�B
�B
�B
 �B
 �B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
 �B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
 �B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
B
B
B
B
B
	B
B
B
B

B
B
	B

B
	B
	B

B

B

B

B

B

B

B

B

B
	B
	B
	B
	B
	B
	B
	B
	B
	B
	B

B

B

B

B

B

B

B
B
B
B
B
B
$B
4B
5B
5B
5B
5B
1B
/B
(B
'B
)B
+B
)B
)B
.B
3B
3B
6B
4B
6B
6B
6B
5B
;B
<B
;B
CB
BB
CB
DB
HB
HB
HB
HB
MB
DB
PB
MB
OB
OB
MB
NB
[B
_B
fB
hB
dB
eB
eB
fB
�B
�B
�B
�B
�B
�G�O�B
"�B
%�B
*�B
4B
8*B
=HB
DuB
H�B
K�B
O�B
V�B
\ B
`B
e611111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111411111111111111 <#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
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
PRES            TEMP            PSAL            PRES_ADJUSTED = PRES - dP                                                                                                                                                                                                                                       none                                                                                                                                                                                                                                                            PSAL_ADJUSTED = sw_salt( sw_cndr(PSAL,TEMP,PRES), TEMP, PRES_ADJUSTED )                                                                                                                                                                                         dP =-0.35 dbar.                                                                                                                                                                                                                                                 none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            Pressures adjusted by using pressure offset at the sea surface. The quoted error is manufacturer specified accuracy in dbar.                                                                                                                                    The quoted error is manufacturer specified accuracy with respect to ITS-90 at time of laboratory calibration.                                                                                                                                                   No significant salinity drift detected. Salinity adjusted for effects of pressure adjustment. The quoted error is max[0.01, 1xOW uncertainty] in PSS-78.                                                                                                        201608071417382016080714173820160807141738  AO  ARCAADJP                                                                    20150627191812    IP                G�O�G�O�G�O�                AO  ARGQQCPL                                                                    20150627191812  QCP$                G�O�G�O�G�O�5F03E           AO  ARGQQCPL                                                                    20150627191812  QCF$                G�O�G�O�G�O�0               UW  ARSQUWQC    WOD & nearby Argo as visual check                               20160807141738  IP                  G�O�G�O�G�O�                