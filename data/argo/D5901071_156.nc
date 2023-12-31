CDF      
      	DATE_TIME         STRING2       STRING4       STRING8       STRING16      STRING32       STRING64   @   	STRING256         N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       D2011-11-30 AOML 2.2 creation; 2015-12-13T22:40:34Z UW 3.1 conversion   
references        (http://www.argodatamgt.org/Documentation   comment       	free text      user_manual_version       3.1    Conventions       Argo-3.1 CF-1.6    featureType       trajectoryProfile         @   	DATA_TYPE                  	long_name         	Data type      conventions       Argo reference table 1     
_FillValue                    6�   FORMAT_VERSION                 	long_name         File format version    
_FillValue                    6�   HANDBOOK_VERSION               	long_name         Data handbook version      
_FillValue                    6�   REFERENCE_DATE_TIME                 	long_name         !Date of reference for Julian days      conventions       YYYYMMDDHHMISS     
_FillValue                    6�   PLATFORM_NUMBER                   	long_name         Float unique identifier    conventions       WMO float identifier : A9IIIII     
_FillValue                    6�   PROJECT_NAME                  	long_name         Name of the project    
_FillValue                  @  6�   PI_NAME                   	long_name         "Name of the principal investigator     
_FillValue                  @  78   STATION_PARAMETERS           	            	long_name         ,List of available parameters for the station   conventions       Argo reference table 3     
_FillValue                  0  7x   CYCLE_NUMBER               	long_name         Float cycle number     conventions       =0...N, 0 : launch cycle (if exists), 1 : first complete cycle      
_FillValue         ��        7�   	DIRECTION                  	long_name         !Direction of the station profiles      conventions       -A: ascending profiles, D: descending profiles      
_FillValue                    7�   DATA_CENTRE                   	long_name         .Data centre in charge of float data processing     conventions       Argo reference table 4     
_FillValue                    7�   DATE_CREATION                   	long_name         Date of file creation      conventions       YYYYMMDDHHMISS     
_FillValue                    7�   DATE_UPDATE                 	long_name         Date of update of this file    conventions       YYYYMMDDHHMISS     
_FillValue                    7�   DC_REFERENCE                  	long_name         (Station unique identifier in data centre   conventions       Data centre convention     
_FillValue                     7�   DATA_STATE_INDICATOR                  	long_name         1Degree of processing the data have passed through      conventions       Argo reference table 6     
_FillValue                    7�   	DATA_MODE                  	long_name         Delayed mode or real time data     conventions       >R : real time; D : delayed mode; A : real time with adjustment     
_FillValue                    7�   PLATFORM_TYPE                     	long_name         Type of float      conventions       Argo reference table 23    
_FillValue                     7�   FLOAT_SERIAL_NO                   	long_name         Serial number of the float     
_FillValue                     8   FIRMWARE_VERSION                  	long_name         Instrument firmware version    
_FillValue                     8<   WMO_INST_TYPE                     	long_name         Coded instrument type      conventions       Argo reference table 8     
_FillValue                    8\   JULD               	long_name         ?Julian day (UTC) of the station relative to REFERENCE_DATE_TIME    standard_name         time   units         "days since 1950-01-01 00:00:00 UTC     conventions       8Relative julian days with decimal part (as parts of day)   
resolution        >Ey��0�:   
_FillValue        A.�~       axis      T           8`   JULD_QC                	long_name         Quality on date and time   conventions       Argo reference table 2     
_FillValue                    8h   JULD_LOCATION                  	long_name         @Julian day (UTC) of the location relative to REFERENCE_DATE_TIME   units         "days since 1950-01-01 00:00:00 UTC     conventions       8Relative julian days with decimal part (as parts of day)   
resolution        >Ey��0�:   
_FillValue        A.�~            8l   LATITUDE               	long_name         &Latitude of the station, best estimate     standard_name         latitude   units         degree_north   
_FillValue        @�i�       	valid_min         �V�        	valid_max         @V�        axis      Y           8t   	LONGITUDE                  	long_name         'Longitude of the station, best estimate    standard_name         	longitude      units         degree_east    
_FillValue        @�i�       	valid_min         �f�        	valid_max         @f�        axis      X           8|   POSITION_QC                	long_name         ,Quality on position (latitude and longitude)   conventions       Argo reference table 2     
_FillValue                    8�   POSITIONING_SYSTEM                    	long_name         Positioning system     
_FillValue                    8�   VERTICAL_SAMPLING_SCHEME                  	long_name         Vertical sampling scheme   conventions       Argo reference table 16    
_FillValue                    8�   CONFIG_MISSION_NUMBER                  	long_name         :Unique number denoting the missions performed by the float     conventions       !1...N, 1 : first complete mission      
_FillValue         ��        9�   PROFILE_PRES_QC                	long_name         #Global quality flag of PRES profile    conventions       Argo reference table 2a    
_FillValue                    9�   PROFILE_TEMP_QC                	long_name         #Global quality flag of TEMP profile    conventions       Argo reference table 2a    
_FillValue                    9�   PROFILE_PSAL_QC                	long_name         #Global quality flag of PSAL profile    conventions       Argo reference table 2a    
_FillValue                    9�   PRES         
      
   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     units         decibar    	valid_min                	valid_max         F;�    
resolution        =���   C_format      %7.1f      FORTRAN_format        F7.1f      
_FillValue        G�O�   axis      Z        �  9�   PRES_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  A�   PRES_ADJUSTED            
      	   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     units         decibar    	valid_min                	valid_max         F;�    
resolution        =���   C_format      %7.1f      FORTRAN_format        F7.1f      
_FillValue        G�O�     �  C�   PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  K|   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     units         decibar    
resolution        =���   C_format      %7.1f      FORTRAN_format        F7.1f      
_FillValue        G�O�     �  Mx   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     	valid_min         �      	valid_max         B      
resolution        :�o   C_format      %9.3f      FORTRAN_format        F9.3f      
_FillValue        G�O�     �  Uh   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ]X   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     	valid_min         �      	valid_max         B      
resolution        :�o   C_format      %9.3f      FORTRAN_format        F9.3f      
_FillValue        G�O�     �  _T   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  gD   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     units         degree_Celsius     
resolution        :�o   C_format      %9.3f      FORTRAN_format        F9.3f      
_FillValue        G�O�     �  i@   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    	valid_min         @      	valid_max         B$     
resolution        :�o   C_format      %9.3f      FORTRAN_format        F9.3f      
_FillValue        G�O�     �  q0   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  y    PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    	valid_min         @      	valid_max         B$     
resolution        :�o   C_format      %9.3f      FORTRAN_format        F9.3f      
_FillValue        G�O�     �  {   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     units         psu    
resolution        :�o   C_format      %9.3f      FORTRAN_format        F9.3f      
_FillValue        G�O�     �  �   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  0  ��   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                    �(   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                    �(   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                    �(   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  ,  �(   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                    �T   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                    �X   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                    �\   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                    �`   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  @  �d   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                    ��   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                    ��   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    ��   HISTORY_START_PRES                    	long_name          Start pressure action applied on   units         decibar    
_FillValue        G�O�        ��   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    units         decibar    
_FillValue        G�O�        ��   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�        ��   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    ��Argo profile    3.1 1.2 19500101000000  5901071 US ARGO PROJECT                                                 Stephen Riser                                                   PRES            TEMP            PSAL               �A   AO  20111130142717  20190522121827  1727_5046_156                   2C  D   APEX                            2143                            040306                          846 @��M���1   @��M�� @5��O�;d�c��1'1   GPS     Primary sampling: mixed [deeper than nominal 980dbar: discrete; nominal 980dbar to surface: 2dbar-bin averaged]                                                                                                                                                    A   A   A   @�33@�  A   A   A@  A`  A�  A�  A�  A�  A�  A�  A�  A�  B   B  B  B  B   B(  B0  B8  B@  BH  BPffBX  B`  Bh  Bp  Bx  B�  B�  B�  B���B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B���B���B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  C   C  C  C  C  C
  C  C  C  C�fC  C  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@�CB�CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C��3C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
�fD  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*y�D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<y�D=  D=� D>  D>� D?  D?� D@  D@� DA  DAy�DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc�fDd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Dsy�Dt  Dy�fD�,�D�\�D��fD�� D�,�D�p D���D�ٚD��D�S3D�� D���D�3D�p DڦfD��fD��D�VfD�D��1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111@�ff@�33A��A!��AA��Aa��A���A���A���A���A���A���A���A���B ffBffBffBffB ffB(ffB0ffB8ffB@ffBHffBP��BXffB`ffBhffBpffBxffB�33B�33B�33B�  B�33B�33B�33B�33B�33B�33B�33B�33B�33B�33B�  B�  B�33B�33B�33B�33B�33B�33B�33B�33B�33B�33B�33B�33B�33B�33B�33B�33C �C�C�C�C�C
�C�C�C�C  C�C�C�C�C�C�C �C"�C$�C&�C(�C*�C,�C.�C0�C2�C4�C6�C8�C:�C<�C>�C@33CB33CD�CF�CH�CJ�CL�CN�CP�CR�CT�CV�CX�CZ�C\�C^�C`�Cb�Cd�Cf�Ch�Cj�Cl�Cn�Cp�Cr�Ct�Cv�Cx�Cz�C|�C~�C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C�  C�  C�  C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��D fD �fDfD�fDfD�fDfD�fDfD�fDfD�fDfD�fDfD�fDfD�fD	fD	�fD
fD
��DfD�fDfD�fDfD�fDfD�fDfD�fDfD�fDfD�fDfD�fDfD�fDfD�fDfD�fDfD�fDfD�fDfD�fDfD�fDfD�fDfD�fDfD�fDfD�fDfD�fDfD�fD fD �fD!fD!�fD"fD"�fD#fD#�fD$fD$�fD%fD%�fD&fD&�fD'fD'�fD(fD(�fD)fD)�fD*fD*� D+fD+�fD,fD,�fD-fD-�fD.fD.�fD/fD/�fD0fD0�fD1fD1�fD2fD2�fD3fD3�fD4fD4�fD5fD5�fD6fD6�fD7fD7�fD8fD8�fD9fD9�fD:fD:�fD;fD;�fD<fD<� D=fD=�fD>fD>�fD?fD?�fD@fD@�fDAfDA� DBfDB�fDCfDC�fDDfDD�fDEfDE�fDFfDF�fDGfDG�fDHfDH�fDIfDI�fDJfDJ�fDKfDK�fDLfDL�fDMfDM�fDNfDN�fDOfDO�fDPfDP�fDQfDQ�fDRfDR�fDSfDS�fDTfDT�fDUfDU�fDVfDV�fDWfDW�fDXfDX�fDYfDY�fDZfDZ�fD[fD[�fD\fD\�fD]fD]�fD^fD^�fD_fD_�fD`fD`�fDafDa�fDbfDb�fDcfDc��DdfDd�fDefDe�fDffDf�fDgfDg�fDhfDh�fDifDi�fDjfDj�fDkfDk�fDlfDl�fDmfDm�fDnfDn�fDofDo�fDpfDp�fDqfDq�fDrfDr�fDsfDs� DtfDy��D�0 D�` D���D��3D�0 D�s3D���D���D�  D�VfD��3D���D�fD�s3Dک�D��D��D�Y�D��D� 1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��A�1'A���A΋DA�G�A�&�A���A��A��TA�ƨA�G�A�A�A˃A��A��/A��Aƣ�A�&�A�jA��A�ƨAå�A�/A�p�A�(�A��A�t�A�t�A��FA�`BA�ƨA�\)A��hA���A��DA�l�A��HA���A�|�A�$�A��wA��DA�n�A�p�A�E�A�t�A��A�  A���A��A�ĜA��\A�S�A���A���A�M�A�oA�ĜA���A�&�A�p�A��\A�(�A��A��TA��HA���A���A��7A�ĜA�+A���A��-A��!A�p�A�33A� �A�oA�?}A��!A�r�A���A��uA���A�$�A�ƨA�VA��A���A�ZA�ƨA�bNA�VA���A��PA�ȴA�O�A��\A�=qA��mA�  A�+A�%A���A�\)A�ffA��PA���A��-A�ZA�  A��A�$�A�-A�  A���A�E�A�{A�I�A��yA���A��A���A�=qA~��Az�HAwp�At�uAs�7ArE�Aq�7Ao�Am��Ajv�Ag;dAe;dAb�A`�A]�TAZA�AW\)AV=qAU?}ATĜAQ�APANE�AM"�ALZAJ��AI��AG��AE/AB��A@�yA@�A@Q�A?
=A>1A=
=A<$�A:��A9�^A8r�A7�A6 �A6�A5S�A4ZA0��A/x�A.�A. �A-?}A+�PA*ĜA)�FA(ȴA'�hA'�A'VA&�A&�9A$��A$$�A#XA!�#A r�A��A�uA5?A��AG�A$�A��AffA9XAoA�#Ax�A/A�mA�A��A�AJA�PA"�A�;A"�A=qA��A
~�A	ƨA	|�A	?}A	A�9Av�AffA-A;dAJAA�A��A-AAp�A ~�@�;d@�v�@��D@�o@��@��H@���@�=q@��^@�%@��
@���@�@@��m@�V@�r�@�1'@睲@��@�7@�7L@�/@�&�@���@䛦@�Q�@◍@���@�@݉7@���@�{@�E�@Ձ@��@ՙ�@�7L@�9X@��@�7L@��/@��/@�V@�&�@��@��@��@���@мj@�bN@϶F@��@��@��@�dZ@ʸR@�@î@å�@��@��;@§�@��@�E�@ļj@�E�@���@�@�@�~�@š�@��@�t�@��y@���@�@�5?@��@�{@��T@�O�@�7L@��@���@�+@�@�@��T@��@��7@���@���@�x�@�X@�/@��`@�Z@���@���@��;@��
@�@�ff@�=q@�{@���@�1@�E�@�@��/@��P@��@�^5@��w@��H@�~�@�ff@�M�@���@�?}@�z�@�?}@���@��R@�b@���@�j@�&�@��P@��@���@��/@�G�@��-@��-@�&�@��@��@�`B@�A�@���@���@�x�@�7L@���@���@�=q@�p�@��/@�z�@�Z@��@���@�;d@�o@��H@���@�n�@���@�7L@�&�@�V@��/@���@�bN@�bN@�j@�bN@�9X@�(�@��@�dZ@�C�@�C�@�;d@�;d@�;d@�;d@�+@���@��@�J@��@���@��h@�x�@�G�@�?}@�?}@�?}@��@���@��/@�V@�&�@�/@�?}@�?}@�7L@��@��@���@�z�@�Z@�I�@��;@��@�\)@�;d@�"�@��@�o@�@���@���@��y@���@��R@���@�E�@��#@���@�hs@��@���@�A�@��@���@�ƨ@�ƨ@���@�@��@��R@�v�@�J@��7@�`B@��@��@��j@���@��u@��D@��@�9X@���@�S�@�o@���@���@���@�v�@�M�@�5?@�-@���@���@��@��\@}�@q��@i��@b��@WK�@O�@E�@A�#@>5?@81'@.��@(Ĝ@#��@��@��@��@J@j@o1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111A�1'A���A΋DA�G�A�&�A���A��A��TA�ƨA�G�A�A�A˃A��A��/A��Aƣ�A�&�A�jA��A�ƨAå�A�/A�p�A�(�A��A�t�A�t�A��FA�`BA�ƨA�\)A��hA���A��DA�l�A��HA���A�|�A�$�A��wA��DA�n�A�p�A�E�A�t�A��A�  A���A��A�ĜA��\A�S�A���A���A�M�A�oA�ĜA���A�&�A�p�A��\A�(�A��A��TA��HA���A���A��7A�ĜA�+A���A��-A��!A�p�A�33A� �A�oA�?}A��!A�r�A���A��uA���A�$�A�ƨA�VA��A���A�ZA�ƨA�bNA�VA���A��PA�ȴA�O�A��\A�=qA��mA�  A�+A�%A���A�\)A�ffA��PA���A��-A�ZA�  A��A�$�A�-A�  A���A�E�A�{A�I�A��yA���A��A���A�=qA~��Az�HAwp�At�uAs�7ArE�Aq�7Ao�Am��Ajv�Ag;dAe;dAb�A`�A]�TAZA�AW\)AV=qAU?}ATĜAQ�APANE�AM"�ALZAJ��AI��AG��AE/AB��A@�yA@�A@Q�A?
=A>1A=
=A<$�A:��A9�^A8r�A7�A6 �A6�A5S�A4ZA0��A/x�A.�A. �A-?}A+�PA*ĜA)�FA(ȴA'�hA'�A'VA&�A&�9A$��A$$�A#XA!�#A r�A��A�uA5?A��AG�A$�A��AffA9XAoA�#Ax�A/A�mA�A��A�AJA�PA"�A�;A"�A=qA��A
~�A	ƨA	|�A	?}A	A�9Av�AffA-A;dAJAA�A��A-AAp�A ~�@�;d@�v�@��D@�o@��@��H@���@�=q@��^@�%@��
@���@�@@��m@�V@�r�@�1'@睲@��@�7@�7L@�/@�&�@���@䛦@�Q�@◍@���@�@݉7@���@�{@�E�@Ձ@��@ՙ�@�7L@�9X@��@�7L@��/@��/@�V@�&�@��@��@��@���@мj@�bN@϶F@��@��@��@�dZ@ʸR@�@î@å�@��@��;@§�@��@�E�@ļj@�E�@���@�@�@�~�@š�@��@�t�@��y@���@�@�5?@��@�{@��T@�O�@�7L@��@���@�+@�@�@��T@��@��7@���@���@�x�@�X@�/@��`@�Z@���@���@��;@��
@�@�ff@�=q@�{@���@�1@�E�@�@��/@��P@��@�^5@��w@��H@�~�@�ff@�M�@���@�?}@�z�@�?}@���@��R@�b@���@�j@�&�@��P@��@���@��/@�G�@��-@��-@�&�@��@��@�`B@�A�@���@���@�x�@�7L@���@���@�=q@�p�@��/@�z�@�Z@��@���@�;d@�o@��H@���@�n�@���@�7L@�&�@�V@��/@���@�bN@�bN@�j@�bN@�9X@�(�@��@�dZ@�C�@�C�@�;d@�;d@�;d@�;d@�+@���@��@�J@��@���@��h@�x�@�G�@�?}@�?}@�?}@��@���@��/@�V@�&�@�/@�?}@�?}@�7L@��@��@���@�z�@�Z@�I�@��;@��@�\)@�;d@�"�@��@�o@�@���@���@��y@���@��R@���@�E�@��#@���@�hs@��@���@�A�@��@���@�ƨ@�ƨ@���@�@��@��R@�v�@�J@��7@�`B@��@��@��j@���@��u@��D@��@�9X@���@�S�@�o@���@���@���@�v�@�M�@�5?@�-@���@���@��@��\@}�@q��@i��@b��@WK�@O�@E�@A�#@>5?@81'@.��@(Ĝ@#��@��@��@��@J@j@o1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oB%BBBB
=BDBJBPBDB%BBBPBhB�B)�B7LB,B33BJ�B?}BE�BJ�BM�B<jBJ�BI�BC�B33B33B7LB9XBK�B\)BcTBaHB^5BcTBhsBk�B|�B�B�+B�JB��B��B��B��B��B��B��B��B�B�B�!B�'B�3B�?B�9B�B�B�B�B�B�RB�FB�B��B�{B�VB�7B�B|�Bx�Bw�Bv�Bs�Bl�Be`B]/BW
BP�BH�BB�B=qB6FB1'B&�B�B{B
=B�B�;B�
B��B�XB�B��B��B�DB�B� Bu�BYB8RB�BoBbB	7BB
�B
�5B
��B
��B
� B
x�B
r�B
dZB
N�B
K�B
C�B
5?B
+B
�B	��B	�ZB	�B	ɺB	��B	�RB	�B	��B	~�B	m�B	^5B	M�B	:^B	+B	�B	B��B��B��B�mB�;B�B��B��BƨB��B�RB��B��B��B�RBǮBĜB�}B�dB�RB�9B�B��B��B�B�XB�^B�9B�'B�'B�B��B��B��B��B��B�bB�DB�7B�7B�7B�%B�B~�B{�By�Bx�Bu�Bw�Bu�Bx�B~�B}�B{�Bz�Bx�Bw�Bw�Bv�Bt�Bs�Bp�Bn�BjBn�Bo�Bt�By�Bx�Bv�Br�Bn�BiyBgmBffBe`BdZBcTBbNB`BB\)BYBT�BS�BVB[#B`BBe`BcTBaHBbNBdZBe`Be`Be`Be`Be`Be`BcTB`BBZBS�BM�BN�BVBW
BVBW
B]/BhsBp�Bv�Bw�Bz�B�JB�\B�{B�oB�VB�7B}�Bv�B�PB��B��B��B��B��B�B�B�'B�9B�XB�XB�dB�qB�wB�wB��BÖBŢBÖB��B��B�dB�9B�-B�3B�?B�RB�9B�?B��B�
B�TB�mB�B�B��B��B��B��B	  B	B	B	1B	
=B	\B	bB	hB	hB	oB	hB	oB	�B	 �B	$�B	+B	2-B	8RB	@�B	G�B	I�B	J�B	K�B	L�B	N�B	T�B	XB	`BB	cTB	cTB	cTB	bNB	bNB	`BB	]/B	\)B	XB	S�B	O�B	J�B	=qB	1'B	1'B	2-B	2-B	0!B	.B	)�B	$�B	$�B	(�B	6FB	D�B	VB	m�B	hsB	hsB	q�B	x�B	z�B	}�B	}�B	x�B	iyB	aHB	XB	VB	^5B	_;B	R�B	S�B	P�B	P�B	R�B	T�B	W
B	YB	YB	YB	^5B	dZB	e`B	e`B	e`B	ffB	jB	n�B	n�B	n�B	p�B	s�B	v�B	w�B	v�B	w�B	w�B	w�B	x�B	z�B	{�B	{�B	{�B	{�B	|�B	{�B	{�B	� B	�B	�%B	�7B	�VB	�uB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�B	�B	�!B	�3B	�9B	�?B	�FB	�FB	�LB	�LB	�LB	�RB	�XB	�XB	�XB	�XB	�RB	�XB	�dB	�dB	�jB	�qB	�wB	��B	B	ÖB	ÖB	ÖB	ÖB	ĜB	ŢB	ŢB	ƨB	ȴB	ɺB	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�B	�)B	�5B	�5B	�5B	�5B	�;B	�HB	�B	��B
1B
�B
 �B
#�B
+B
49B
9XB
=qB
A�B
F�B
M�B
T�B
[#B
_;B
bNB
gmB
m�B
r�B
u�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111BbBBB%BDBDBJBVBPB
=B+BDB�B�B�B,B:^B/B49BO�BA�BI�BL�BQ�B>wBM�BK�BG�B8RB49B:^B=qBK�B]/Be`BbNB_;Be`BjBl�B}�B�B�1B�hB��B��B��B��B��B��B��B��B�B�!B�'B�-B�9B�LB�FB�-B�B�!B�!B�B�XB�^B�3B��B��B�hB�VB�7B}�By�Bw�Bw�Bx�Bp�Bm�B`BBZBW
BK�BE�B@�B;dB7LB)�B �B�B�B��B�NB�)B��B�jB�-B��B��B�VB�B�B� BcTBA�B�B{BoBJB1B
��B
�`B
�BB
��B
�B
z�B
x�B
l�B
O�B
M�B
H�B
8RB
1'B
'�B
B	�B	�B	��B	B	�dB	�B	��B	�%B	r�B	dZB	S�B	@�B	5?B	�B	1B	B��B��B�B�ZB�#B��B��BɺBǮB�}B�!B��B��B�XB��BǮBB�wB�dB�RB�'B�B�B�B�^B�jB�^B�9B�3B�B�B��B��B��B��B�uB�JB�7B�=B�=B�DB�B�B� B}�B{�Bx�Bx�Bv�Bz�B�B�B}�B{�B{�Bz�Bx�Bw�Bx�Bv�Bt�Bp�Bs�Bp�Bq�Bx�B|�B{�Bw�Bw�Bq�BjBhsBgmBffBe`BcTBcTBdZBaHB`BBZBVBW
B]/BdZBhsBe`Be`Be`BdZBe`Be`BffBffBgmBhsBiyBffB^5B[#BR�BO�BW
BXBXBYB^5BhsBp�Bv�Bx�B{�B�VB�oB��B�{B�\B�PB�Bw�B�PB��B��B��B��B��B�B�B�'B�9B�XB�XB�dB�qB�wB�wB��BĜBƨBƨBB��BB�LB�-B�3B�?B�^B�FB�9B�qB��B�NB�mB�B�B��B��B��B��B	  B	B	%B	1B	
=B	\B	hB	hB	hB	uB	uB	{B	�B	 �B	%�B	)�B	2-B	8RB	A�B	G�B	I�B	K�B	L�B	M�B	N�B	T�B	XB	aHB	dZB	cTB	cTB	cTB	e`B	cTB	^5B	^5B	ZB	S�B	P�B	O�B	E�B	2-B	1'B	2-B	33B	1'B	/B	.B	&�B	%�B	&�B	49B	@�B	O�B	o�B	iyB	gmB	p�B	w�B	y�B	}�B	~�B	~�B	l�B	dZB	ZB	S�B	_;B	ffB	S�B	VB	R�B	Q�B	R�B	VB	XB	YB	ZB	ZB	_;B	dZB	e`B	ffB	e`B	gmB	k�B	n�B	n�B	n�B	q�B	s�B	v�B	w�B	v�B	w�B	w�B	x�B	y�B	z�B	{�B	{�B	{�B	{�B	|�B	{�B	|�B	�B	�B	�%B	�7B	�VB	�uB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�B	�B	�'B	�3B	�9B	�?B	�FB	�FB	�LB	�LB	�LB	�RB	�XB	�XB	�XB	�XB	�RB	�^B	�dB	�dB	�jB	�qB	�wB	��B	B	ÖB	ÖB	ÖB	ĜB	ĜB	ŢB	ŢB	ǮB	ɺB	ɺB	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�B	�)B	�5B	�5B	�5B	�5B	�;B	�HB	�B	��B
1B
�B
 �B
#�B
+B
49B
9XB
=qB
A�B
F�B
L�B
T�B
[#B
_;B
bNB
gmB
m�B
r�B
u�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<e`B<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<��
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<D��<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
PRES            TEMP            PSAL            PRES_ADJUSTED = PRES - dP                                                                                                                                                                                                                                       none                                                                                                                                                                                                                                                            PSAL_ADJUSTED = sw_salt( sw_cndr(PSAL,TEMP,PRES), TEMP, PRES_ADJUSTED ) + CTM                                                                                                                                                                                   dP =-0.1 dbar.                                                                                                                                                                                                                                                  none                                                                                                                                                                                                                                                            CTM: alpha=0.141, tau=6.68s                                                                                                                                                                                                                                     Pressures adjusted by using pressure offset at the sea surface. The quoted error is manufacturer specified accuracy in dbar.                                                                                                                                    The quoted error is manufacturer specified accuracy with respect to ITS-90 at time of laboratory calibration.                                                                                                                                                   No significant salinity drift detected. Salinity adjusted for effects of pressure adjustment. Conductivity cell thermal mass correction (CTM) applied to measurements taken in Cp mode. The quoted error is max[0.01, sqrt(OWerr^2+CTMerr^2)] in PSS-78.        201201031447282012010314472820120103144728  AO  ARGQ                                                                        20111130142717  QCP$                G�O�G�O�G�O�FFBFE           AO  ARGQ                                                                        20111130142717  QCF$                G�O�G�O�G�O�0               UW  ARSQUWQC    WOD & nearby Argo as visual check                               20120103144729  IP                  G�O�G�O�G�O�                