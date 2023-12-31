CDF      
      	DATE_TIME         STRING2       STRING4       STRING8       STRING16      STRING32       STRING64   @   	STRING256         N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       D2011-11-30 AOML 2.2 creation; 2015-12-13T22:40:29Z UW 3.1 conversion   
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
_FillValue                    ��Argo profile    3.1 1.2 19500101000000  5901071 US ARGO PROJECT                                                 Stephen Riser                                                   PRES            TEMP            PSAL               �A   AO  20111130142235  20190522121827  1727_5046_135                   2C  D   APEX                            2143                            040306                          846 @��*�@ 1   @��+b���@7\�1&��ddZ�1   GPS     Primary sampling: mixed [deeper than nominal 980dbar: discrete; nominal 980dbar to surface: 2dbar-bin averaged]                                                                                                                                                    A   A   A   @�33@�  A   A   A@  A`  A�  A�  A�  A�33A�  A�  A�  A�  B   B  B  B  B   B(  B0  B8  B@  BH  BP  BX  B`  Bh  Bp  Bx  B�  B�  B�  B�  B�  B�  B�  B�  B�33B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B���B�  B�  B�  B�33B�  B�  B�  B�  B�  C   C  C  C  C  C
  C  C  C  C  C  C  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CE�fCH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� DfD� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  Dy�D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'�fD(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DPfDP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm�fDn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dt� Dy�3D�0 D�\�D��3D��3D�)�D�VfD���D��fD�  D�L�D�� D��fD��D�s3Dڠ D���D��D�S3D�f1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111@�ff@�33A��A!��AA��Aa��A���A���A���A�  A���A���A���A���B ffBffBffBffB ffB(ffB0ffB8ffB@ffBHffBPffBXffB`ffBhffBpffBxffB�33B�33B�33B�33B�33B�33B�33B�33B�ffB�33B�33B�33B�33B�33B�33B�33B�33B�33B�33B�33B�33B�33B�  B�33B�33B�33B�ffB�33B�33B�33B�33B�33C �C�C�C�C�C
�C�C�C�C�C�C�C�C�C�C�C �C"�C$�C&�C(�C*�C,�C.�C0�C2�C4�C6�C8�C:�C<�C>�C@�CB�CD�CF  CH�CJ�CL�CN�CP�CR�CT�CV�CX�CZ�C\�C^�C`�Cb�Cd�Cf�Ch�Cj�Cl�Cn�Cp�Cr�Ct�Cv�Cx�Cz�C|�C~�C��C��C��C��C��C��C��C��C��C��C��C��C��C�  C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C�  C��C��C��C��C��C��C��C��C��C��C��C��C�  C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��D fD �fDfD�fDfD�fDfD�fDfD�fDfD�fD�D�fDfD�fDfD�fD	fD	�fD
fD
�fDfD�fDfD�fDfD�fDfD�fDfD�fDfD�fDfD�fDfD�fDfD�fDfD� DfD�fDfD�fDfD�fDfD�fDfD�fDfD�fDfD�fDfD�fDfD�fDfD�fDfD�fD fD �fD!fD!�fD"fD"�fD#fD#�fD$fD$�fD%fD%�fD&fD&�fD'fD'��D(fD(�fD)fD)�fD*fD*�fD+fD+�fD,fD,�fD-fD-�fD.fD.�fD/fD/�fD0fD0�fD1fD1�fD2fD2�fD3fD3�fD4fD4�fD5fD5�fD6fD6�fD7fD7�fD8fD8�fD9fD9�fD:fD:�fD;fD;�fD<fD<�fD=fD=�fD>fD>�fD?fD?�fD@fD@�fDAfDA�fDBfDB�fDCfDC�fDDfDD�fDEfDE�fDFfDF�fDGfDG�fDHfDH�fDIfDI�fDJfDJ�fDKfDK�fDLfDL�fDMfDM�fDNfDN�fDOfDO�fDP�DP�fDQfDQ�fDRfDR�fDSfDS�fDTfDT�fDUfDU�fDVfDV�fDWfDW�fDXfDX�fDYfDY�fDZfDZ�fD[fD[�fD\fD\�fD]fD]�fD^fD^�fD_fD_�fD`fD`�fDafDa�fDbfDb�fDcfDc�fDdfDd�fDefDe�fDffDf�fDgfDg�fDhfDh�fDifDi�fDjfDj�fDkfDk�fDlfDl�fDmfDm��DnfDn�fDofDo�fDpfDp�fDqfDq�fDrfDr�fDsfDs�fDtfDt�fDyٚD�33D�` D��fD��fD�,�D�Y�D���D��D�#3D�P D��3D��D��D�vfDڣ3D�� D�  D�VfD�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��A�$�A�&�A��A� �A�$�A�1'A�9XA�7LA�;dA�7LA�;dA�=qA�=qA�=qA�A�A�A�A�C�A�A�A�A�A�A�A�9XA�1'A�1'A� �A�
=A���A��A��TA���A��RA�S�A�5?A�;dA���A��A�r�A�^5A�&�A��A�bA�1A��HA��wA���A���A��A��yA��FA��jA���A��PA�^5A���A�oA���A�7LA��A��9A��A�l�A�E�A�&�A�A��A��RA��hA�~�A�jA���A���A��jA�v�A�
=A��A�hsA��/A�z�A�z�A�O�A�x�A��+A��9A�&�A�t�A�;dA�K�A��TA��FA� �A���A�ĜA��wA��PA��A��-A�A�A�A�l�A�l�A�
=A�
=A�hsA�/A��7A�K�A���A���A��wA�33A���A��A��9A�5?A��+A��`A�Q�A��-A��DA���A��-A��!A���A�S�A�ffA}��Az��Av�jAuAr1Ap��An1Aj$�Ae�7Ad^5Ac�AbjA`ȴA`E�A^��A^{A]�
A\9XAY�;AY%AX1'AWC�AV��AVffAU�;AUoASG�AQ�hAPI�AO��AM�AL�AK�AJȴAJ�jAJ=qAIdZAHȴAGx�AE�ADE�AC�hAB�!ABn�AA�A@=qA?dZA;�A:�DA9�-A9%A8bNA7�mA7��A6�uA5�#A5;dA4��A4bA3l�A3�A2ĜA1&�A0=qA/�;A/33A.^5A,z�A+&�A*A�A)�#A)oA';dA&�yA&ffA%G�A$�`A$��A$�A$(�A#XA#/A"ffA!�-A ��A�^An�A�-A33An�A��A�\A5?A�A?}A��An�A��A��AO�A&�A%A��A�^AS�A9XA�PAn�A�TA��AM�Al�A+A��A�DAƨA
�`A	l�A�AVA�`A�A��A1A��A33Ap�AA ĜA {@��^@�/@��H@�Z@�"�@��j@�?}@�I�@�|�@�@�1@���@�hs@� �@��/@�(�@�l�@���@��@ް!@݉7@ۅ@�{@��`@֧�@�bN@�K�@���@��@�bN@�&�@��@��;@��;@���@��H@�$�@�l�@�5?@��`@��m@��H@�?}@��#@�=q@��9@���@���@�33@��@��/@� �@���@��\@���@��@��H@��@���@��@��@�  @�33@��\@�-@�p�@��/@�V@�-@�n�@���@��m@�(�@��@��@��
@�\)@��!@���@�t�@�ȴ@�$�@���@�O�@�G�@��@�"�@��@��@��@�{@�$�@���@���@��^@���@��h@�?}@���@��;@��m@�Z@�bN@��
@���@�|�@�33@��^@��`@�z�@�I�@� �@�1'@��`@�V@�(�@��@��@�Q�@��D@��D@�(�@���@���@��
@�ƨ@���@��P@��@�|�@�t�@�dZ@�K�@���@�v�@�M�@�@���@�`B@�&�@�%@���@��/@�  @�+@�S�@�t�@�33@��R@�n�@�-@��@���@���@��7@�O�@���@��9@�bN@��@�S�@���@���@�M�@�J@���@��7@�?}@��@���@�K�@�-@�Ĝ@��u@��9@�Z@��;@��;@��@�l�@�dZ@�ȴ@�=q@�=q@�{@���@���@���@���@�`B@��@�1'@��9@�Ĝ@���@���@�Ĝ@�Q�@��m@�C�@�S�@�l�@�dZ@�+@�o@��\@�^5@�M�@���@�x�@�G�@���@���@��D@�r�@�bN@�A�@� �@��@��m@���@�l�@�;d@�"�@�@��@��y@��H@���@���@��+@�~�@�v�@�v�@��u@�I�@v5?@m�h@aG�@Y�#@R��@J�@B�@=�@5�@/K�@)��@%O�@ bN@��@A�@�@�w@331111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111A�$�A�&�A��A� �A�$�A�1'A�9XA�7LA�;dA�7LA�;dA�=qA�=qA�=qA�A�A�A�A�C�A�A�A�A�A�A�A�9XA�1'A�1'A� �A�
=A���A��A��TA���A��RA�S�A�5?A�;dA���A��A�r�A�^5A�&�A��A�bA�1A��HA��wA���A���A��A��yA��FA��jA���A��PA�^5A���A�oA���A�7LA��A��9A��A�l�A�E�A�&�A�A��A��RA��hA�~�A�jA���A���A��jA�v�A�
=A��A�hsA��/A�z�A�z�A�O�A�x�A��+A��9A�&�A�t�A�;dA�K�A��TA��FA� �A���A�ĜA��wA��PA��A��-A�A�A�A�l�A�l�A�
=A�
=A�hsA�/A��7A�K�A���A���A��wA�33A���A��A��9A�5?A��+A��`A�Q�A��-A��DA���A��-A��!A���A�S�A�ffA}��Az��Av�jAuAr1Ap��An1Aj$�Ae�7Ad^5Ac�AbjA`ȴA`E�A^��A^{A]�
A\9XAY�;AY%AX1'AWC�AV��AVffAU�;AUoASG�AQ�hAPI�AO��AM�AL�AK�AJȴAJ�jAJ=qAIdZAHȴAGx�AE�ADE�AC�hAB�!ABn�AA�A@=qA?dZA;�A:�DA9�-A9%A8bNA7�mA7��A6�uA5�#A5;dA4��A4bA3l�A3�A2ĜA1&�A0=qA/�;A/33A.^5A,z�A+&�A*A�A)�#A)oA';dA&�yA&ffA%G�A$�`A$��A$�A$(�A#XA#/A"ffA!�-A ��A�^An�A�-A33An�A��A�\A5?A�A?}A��An�A��A��AO�A&�A%A��A�^AS�A9XA�PAn�A�TA��AM�Al�A+A��A�DAƨA
�`A	l�A�AVA�`A�A��A1A��A33Ap�AA ĜA {@��^@�/@��H@�Z@�"�@��j@�?}@�I�@�|�@�@�1@���@�hs@� �@��/@�(�@�l�@���@��@ް!@݉7@ۅ@�{@��`@֧�@�bN@�K�@���@��@�bN@�&�@��@��;@��;@���@��H@�$�@�l�@�5?@��`@��m@��H@�?}@��#@�=q@��9@���@���@�33@��@��/@� �@���@��\@���@��@��H@��@���@��@��@�  @�33@��\@�-@�p�@��/@�V@�-@�n�@���@��m@�(�@��@��@��
@�\)@��!@���@�t�@�ȴ@�$�@���@�O�@�G�@��@�"�@��@��@��@�{@�$�@���@���@��^@���@��h@�?}@���@��;@��m@�Z@�bN@��
@���@�|�@�33@��^@��`@�z�@�I�@� �@�1'@��`@�V@�(�@��@��@�Q�@��D@��D@�(�@���@���@��
@�ƨ@���@��P@��@�|�@�t�@�dZ@�K�@���@�v�@�M�@�@���@�`B@�&�@�%@���@��/@�  @�+@�S�@�t�@�33@��R@�n�@�-@��@���@���@��7@�O�@���@��9@�bN@��@�S�@���@���@�M�@�J@���@��7@�?}@��@���@�K�@�-@�Ĝ@��u@��9@�Z@��;@��;@��@�l�@�dZ@�ȴ@�=q@�=q@�{@���@���@���@���@�`B@��@�1'@��9@�Ĝ@���@���@�Ĝ@�Q�@��m@�C�@�S�@�l�@�dZ@�+@�o@��\@�^5@�M�@���@�x�@�G�@���@���@��D@�r�@�bN@�A�@� �@��@��m@���@�l�@�;d@�"�@�@��@��y@��H@���@���@��+@�~�@�v�@�v�@��u@�I�@v5?@m�h@aG�@Y�#@R��@J�@B�@=�@5�@/K�@)��@%O�@ bN@��@A�@�@�w@331111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oB�JB�JB�JB�JB�JB�JB�JB�JB�JB�JB�JB�JB�JB�JB�JB�JB�JB�JB�PB�PB�VB�bB�bB�{B��B��B��B��B�B��B�B�!BBÖBÖBÖBŢBɺB�ZB�B�B��B��B��B�B+B,B)�B7LBD�B>wB>wB?}B>wB>wB?}BB�BD�BE�BF�BG�BG�BI�BJ�BN�BQ�BR�BW
BcTBq�B�B�Bx�BhsB^5B[#BXBZBW
BJ�B9XB%�B�B	7B��B�BI�B�B�B�B�B�BuBPB1BB��B�HB��B�9B��B��B�JB�Bm�BL�B5?B+B�B{BJB%B
��B
�B
�ZB
�/B
�B
ɺB
�wB
�dB
�B
��B
�B
q�B
J�B
1'B
9XB
9XB
PB	��B	�B	�9B	�{B	�{B	�hB	�+B	x�B	r�B	iyB	e`B	e`B	[#B	K�B	H�B	E�B	C�B	C�B	@�B	=qB	7LB	1'B	'�B	#�B	�B	�B	uB	hB	bB	\B	PB	
=B	%B	B��B��B��B��B��B�B�B�B�HB�B�
B�B�#B�B�B��B��BŢBB�}B�jB�XB�FB�B�B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B�oB�\B�VB�PB�JB�bB�hB�VB�=B�+B�%B�%B�+B�+B�+B�=B�1B�B�B~�B}�B~�B}�B}�B|�Bz�Bw�Bt�Br�Br�Bq�Bo�Bn�Bm�Bl�BjBgmBdZBbNB`BB`BB_;B_;B]/B[#BYBVBW
BVBT�BO�BO�BO�BL�BI�BE�BB�B@�B?}BA�B@�B@�B>wB>wB;dB;dB@�B@�BH�BF�BE�BH�BH�BF�BA�B?}BB�BD�BG�BJ�BI�BL�BQ�BT�B[#B`BBe`BffBffBiyBe`BjBn�BjBjBy�B~�B~�B~�B~�B~�B|�B}�B� B�B�B�%B�PB�oB�bB�JB�=B�7B�7B�=B�DB�\B�hB��B��B��B��B�B�FB�^B�jB�jB�dB�XB�FB�9B�9B�?B�9B�FB�wBÖBÖBƨBɺB��B��B��B��B��B��B�B�B�B�#B�5B�HB�mB�yB�B�B�B�B�B��B��B��B��B	B	VB	\B	hB	{B	�B	�B	�B	!�B	%�B	'�B	(�B	+B	,B	/B	0!B	1'B	2-B	2-B	33B	49B	9XB	;dB	;dB	;dB	;dB	;dB	>wB	?}B	?}B	@�B	B�B	D�B	H�B	M�B	O�B	S�B	VB	XB	YB	ZB	[#B	\)B	^5B	`BB	aHB	dZB	iyB	l�B	m�B	o�B	r�B	s�B	r�B	u�B	w�B	x�B	x�B	z�B	w�B	x�B	{�B	� B	�B	�B	�B	�B	�%B	�+B	�=B	�DB	�DB	�JB	�VB	�uB	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�B	�B	�!B	�-B	�-B	�9B	�?B	�?B	�9B	�9B	�RB	�^B	�^B	�dB	�qB	�}B	��B	��B	��B	B	ÖB	ĜB	ŢB	ƨB	ǮB	ǮB	ȴB	ȴB	ȴB	ɺB	ɺB	ɺB	��B	��B	��B	�B	��B
B
VB
�B
 �B
(�B
33B
;dB
B�B
G�B
M�B
R�B
[#B
^5B
cTB
gmB
m�B
r�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111B�JB�JB�JB�JB�JB�JB�JB�JB�JB�JB�JB�JB�JB�JB�JB�JB�JB�JB�PB�PB�VB�bB�bB�{B��B��B��B��B�B�B�!B�XBŢBȴBŢBĜBƨBɺB�ZB�B��B��B��B��B�B+B-B)�B7LBF�B?}BA�BE�BA�BA�BA�BD�BE�BF�BG�BH�BH�BI�BK�BO�BQ�BR�BVBbNBq�B�B�B|�Bk�BaHB]/BXB[#B[#BO�B=qB(�B�B�B�BB�oBYB!�B�B�B�B�B�B\B
=BB��B�BÖB�XB��B��B�\B�+Bz�BW
B:^B33B�B�BVB	7B+B
�B
�mB
�BB
�#B
��B
�wB
�}B
�FB
��B
�+B
x�B
P�B
:^B
;dB
B�B
bB
B	�TB	�}B	��B	��B	��B	�JB	z�B	v�B	k�B	ffB	jB	bNB	N�B	K�B	H�B	E�B	D�B	B�B	@�B	=qB	7LB	,B	&�B	%�B	�B	�B	oB	bB	hB	bB	JB	DB	+B	B��B��B��B��B��B�B��B�`B�/B�B�B�)B�#B�#B��B��BǮBĜB��B�qB�^B�^B�'B�B�B��B��B��B��B��B��B��B��B��B��B��B��B��B��B�{B�bB�hB�\B�\B�uB��B�bB�JB�=B�1B�=B�1B�1B�7B�DB�=B�=B�B�B~�B� B� B�B~�B~�By�Bx�Bt�Bs�Bu�Br�Bo�Bn�Bm�Bm�BjBiyBgmBcTBaHB_;B`BB_;B^5B]/B[#BXBW
BW
BR�BP�BR�BP�BK�BI�BG�BB�BA�BD�BB�BB�B@�B>wBA�B<jBB�BD�BJ�BI�BG�BL�BK�BH�BE�BC�BD�BE�BI�BM�BN�BN�BQ�BT�B[#BbNBgmBk�BhsBk�BgmBl�Bn�Bp�BjB|�B� B� B� B�B�B}�B~�B�B�B�B�B�PB��B�{B�PB�DB�=B�=B�DB�JB�bB�hB��B��B��B��B�B�FB�^B�qB�qB�jB�dB�^B�9B�?B�FB�?B�FB��BƨBŢBƨBɺB��B��B��B��B��B��B�B�B�B�#B�5B�BB�mB�B�B�B�B�B�B��B��B��B��B	B	VB	bB	oB	{B	�B	�B	�B	"�B	%�B	'�B	(�B	+B	,B	/B	0!B	1'B	2-B	2-B	33B	5?B	:^B	;dB	<jB	;dB	<jB	;dB	>wB	?}B	?}B	A�B	C�B	D�B	H�B	N�B	P�B	T�B	VB	XB	YB	ZB	[#B	\)B	_;B	aHB	bNB	e`B	jB	l�B	n�B	p�B	r�B	s�B	s�B	v�B	w�B	y�B	z�B	|�B	y�B	x�B	{�B	�B	�B	�B	�B	�B	�%B	�1B	�DB	�DB	�DB	�PB	�VB	�uB	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�B	�B	�!B	�-B	�3B	�9B	�?B	�FB	�?B	�9B	�XB	�^B	�^B	�dB	�qB	�}B	��B	��B	��B	B	ÖB	ĜB	ŢB	ƨB	ǮB	ǮB	ȴB	ȴB	ȴB	ɺB	ɺB	ɺB	��B	��B	��B	�yB	��B
B
VB
�B
 �B
(�B
33B
;dB
B�B
G�B
M�B
R�B
[#B
^5B
bNB
gmB
m�B
r�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<�o<��
<�C�<u<#�
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
<T��<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<49X<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
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
PRES            TEMP            PSAL            PRES_ADJUSTED = PRES - dP                                                                                                                                                                                                                                       none                                                                                                                                                                                                                                                            PSAL_ADJUSTED = sw_salt( sw_cndr(PSAL,TEMP,PRES), TEMP, PRES_ADJUSTED ) + CTM                                                                                                                                                                                   dP =-0.1 dbar.                                                                                                                                                                                                                                                  none                                                                                                                                                                                                                                                            CTM: alpha=0.141, tau=6.68s                                                                                                                                                                                                                                     Pressures adjusted by using pressure offset at the sea surface. The quoted error is manufacturer specified accuracy in dbar.                                                                                                                                    The quoted error is manufacturer specified accuracy with respect to ITS-90 at time of laboratory calibration.                                                                                                                                                   No significant salinity drift detected. Salinity adjusted for effects of pressure adjustment. Conductivity cell thermal mass correction (CTM) applied to measurements taken in Cp mode. The quoted error is max[0.01, sqrt(OWerr^2+CTMerr^2)] in PSS-78.        201201031447212012010314472120120103144721  AO  ARGQ                                                                        20111130142235  QCP$                G�O�G�O�G�O�FFBFE           AO  ARGQ                                                                        20111130142235  QCF$                G�O�G�O�G�O�0               UW  ARSQUWQC    WOD & nearby Argo as visual check                               20120103144721  IP                  G�O�G�O�G�O�                