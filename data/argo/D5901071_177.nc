CDF      
      	DATE_TIME         STRING2       STRING4       STRING8       STRING16      STRING32       STRING64   @   	STRING256         N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       D2011-11-30 AOML 2.2 creation; 2015-12-13T22:40:40Z UW 3.1 conversion   
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
_FillValue                    ��Argo profile    3.1 1.2 19500101000000  5901071 US ARGO PROJECT                                                 Stephen Riser                                                   PRES            TEMP            PSAL               �A   AO  20111130143139  20190522121827  1727_5046_177                   2C  D   APEX                            2143                            040306                          846 @��r� 1   @��sO���@4���+�d��Q�1   GPS     Primary sampling: mixed [deeper than nominal 980dbar: discrete; nominal 980dbar to surface: 2dbar-bin averaged]                                                                                                                                                    A   A   A   @�  @�  A   A   A@  Aa��A�  A�  A�  A�  A�  A�  A�  A�  B   B  B  B  B   B(  B0  B8  B@  BHffBPffBX  B_��Bh  Bp  Bx  B�  B�  B�  B�  B�  B�  B�  B�33B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B���C  C  C  C  C
  C  C  C  C  C  C  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH�CJ  CL  CN  CO�fCR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D �fDfD�fD  D� D��Dy�D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  Dy�D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE�fDF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Diy�Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dty�Dy�fD��D�p D���D��fD�#3D�l�D�� D���D�fD�i�D���D��D�#3D�I�Dڠ D���D�&fD�S3D� 1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111@�33@�33A��A!��AA��Ac33A���A���A���A���A���A���A���A���B ffBffBffBffB ffB(ffB0ffB8ffB@ffBH��BP��BXffB`  BhffBpffBxffB�33B�33B�33B�33B�33B�33B�33B�ffB�33B�33B�33B�33B�33B�33B�33B�33B�33B�33B�33B�33B�33B�33B�33B�33B�33B�33B�33B�33B�33B�33B�33B�33C   C�C�C�C�C
�C�C�C�C�C�C�C�C�C�C�C �C"�C$�C&�C(�C*�C,�C.�C0�C2�C4�C6�C8�C:�C<�C>�C@�CB�CD�CF�CH33CJ�CL�CN�CP  CR�CT�CV�CX�CZ�C\�C^�C`�Cb�Cd�Cf�Ch�Cj�Cl�Cn�Cp�Cr�Ct�Cv�Cx�Cz�C|�C~�C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C�  C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��D fD ��D�D��DfD�fD  D� DfD�fDfD�fDfD�fDfD�fDfD�fD	fD	�fD
fD
�fDfD�fDfD�fDfD�fDfD�fDfD�fDfD�fDfD�fDfD�fDfD�fDfD�fDfD�fDfD�fDfD�fDfD� DfD�fDfD�fDfD�fDfD�fDfD�fDfD�fDfD�fD fD �fD!fD!�fD"fD"�fD#fD#�fD$fD$�fD%fD%�fD&fD&�fD'fD'�fD(fD(�fD)fD)�fD*fD*�fD+fD+�fD,fD,�fD-fD-�fD.fD.�fD/fD/�fD0fD0�fD1fD1�fD2fD2�fD3fD3�fD4fD4�fD5fD5�fD6fD6�fD7fD7�fD8fD8�fD9fD9�fD:fD:�fD;fD;�fD<fD<�fD=fD=�fD>fD>�fD?fD?�fD@fD@�fDAfDA�fDBfDB�fDCfDC�fDDfDD�fDEfDE��DFfDF�fDGfDG�fDHfDH�fDIfDI�fDJfDJ�fDKfDK�fDLfDL�fDMfDM�fDNfDN�fDOfDO�fDPfDP�fDQfDQ�fDRfDR�fDSfDS�fDTfDT�fDUfDU�fDVfDV�fDWfDW�fDXfDX�fDYfDY�fDZfDZ�fD[fD[�fD\fD\�fD]fD]�fD^fD^�fD_fD_�fD`fD`�fDafDa�fDbfDb�fDcfDc�fDdfDd�fDefDe�fDffDf�fDgfDg�fDhfDh�fDifDi� DjfDj�fDkfDk�fDlfDl�fDmfDm�fDnfDn�fDofDo�fDpfDp�fDqfDq�fDrfDr�fDsfDs�fDtfDt� Dy��D�  D�s3D���D��D�&fD�p D��3D�� D��D�l�D���D���D�&fD�L�Dڣ3D�� D�)�D�VfD�31111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��A�|�A�|�A�|�A�~�A�~�AӁAӃAӃAӃAӇ+AӉ7AӉ7Aә�Aӝ�Aӝ�Aӟ�Aӟ�Aӛ�Aӛ�AӓuA�Q�A�1'A�^5A�^5A�A�oA��Aź^A�1A�VA�hsA��A�^5A�S�A�K�A�O�A���A�1'A��A�XA��A���A� �A��\A�A��\A��mA��+A�~�A�n�A��9A�C�A�  A���A���A�~�A�l�A�A��!A��A�A��+A�?}A�oA�S�A���A���A��A��mA��9A���A��DA�v�A�G�A�  A���A���A��A���A��jA��A�ZA�I�A�(�A� �A��A���A���A��jA��^A��7A��^A�?}A�z�A��7A��uA��uA��!A���A�+A�oA�G�A���A���A���A�ȴA��A���A��9A�~�A�XA���A�$�A�VA}+Au�Ap~�Ao
=Al��Ak�-Aj��Ai�;AiVAf��Ae�AdI�Ab��Aa��A_"�A]/A[ƨAY��AW7LAV-AU��AS�AP�/AN�uAM
=AJ��AI�AG;dAD�RAC��AC�hACG�AB�9ABAAx�AA&�A>��A<�A:��A9
=A8(�A6jA4��A2ĜA1�A1�A17LA0$�A-��A+x�A+oA*�A*�9A*  A'K�A%"�A$�A$v�A!�-A�TA�Al�AȴA�wA�7A^5AXA��AK�A�A(�A��A/AA�A��Az�AC�A�#AS�AȴA��A
�A	G�AVA��A�-A��AK�A 1'@�{@��7@�O�@��`@��@�t�@��\@�$�@�p�@��h@���@���@���@��/@���@��@ꟾ@�V@��@�V@�|�@�P@��@�|�@�S�@�+@��@��y@�ff@�^@�V@��
@��@�~�@�`B@�+@�(�@�@�z�@��y@�%@�9X@�C�@�
=@���@ҟ�@�hs@�I�@�33@Χ�@�5?@�hs@̋D@�r�@�1'@�\)@�n�@Ɂ@ȼj@ǍP@��y@�V@���@�A�@Å@�o@���@°!@�-@�J@���@�V@�bN@���@��!@�-@��^@��D@�I�@���@���@��@�"�@��\@�^5@�{@�x�@���@���@��@�r�@�I�@���@�;d@�33@�
=@���@�ȴ@��R@���@��+@��@��^@�p�@�G�@���@�r�@�(�@��@���@�5?@��@���@��w@�^5@��-@�?}@�%@�X@�@��-@�/@�G�@��7@��@���@���@�O�@��7@�?}@��@��/@�9X@�1@��P@�^5@��H@�+@���@���@���@�t�@�o@�
=@�=q@�@�p�@�X@�&�@�Z@�K�@��R@�5?@�J@��T@��T@��^@�p�@�7L@�&�@���@��@��@��u@��@�Z@�9X@�9X@�1'@�b@�I�@�A�@�|�@��@��!@��+@�M�@���@��T@�$�@��@��7@���@�%@��h@�ff@��\@�M�@��T@�7L@���@�bN@��@�r�@���@�33@���@�J@���@���@��-@�Z@���@�Ĝ@��9@��@��@���@�5?@�5?@�-@�ff@��!@���@�ȴ@��R@��!@���@�v�@�5?@��@��-@��7@�/@�Ĝ@�Z@�  @��@��F@�S�@�K�@�K�@�;d@�o@��y@��!@���@�ff@�-@�@��@��#@��^@��7@�p�@�/@���@��9@�(�@��P@�dZ@��y@�n�@�5?@�@�V@�t�@�ff@�O�@�Z@��@�
=@�ƨ@�(�@��w@�t�@�C�@�+@�C�@�|�@��@�l�@�+@��y@���@�n�@�J@��@���@���@�X@�G�@�G�@�/@�\)@K�@v{@o;d@g
=@Z�\@T�D@J-@B�@<Z@5V@/��@(��@%?}@"�@��@�R@=q@/@	�71111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111A�|�A�|�A�|�A�~�A�~�AӁAӃAӃAӃAӇ+AӉ7AӉ7Aә�Aӝ�Aӝ�Aӟ�Aӟ�Aӛ�Aӛ�AӓuA�Q�A�1'A�^5A�^5A�A�oA��Aź^A�1A�VA�hsA��A�^5A�S�A�K�A�O�A���A�1'A��A�XA��A���A� �A��\A�A��\A��mA��+A�~�A�n�A��9A�C�A�  A���A���A�~�A�l�A�A��!A��A�A��+A�?}A�oA�S�A���A���A��A��mA��9A���A��DA�v�A�G�A�  A���A���A��A���A��jA��A�ZA�I�A�(�A� �A��A���A���A��jA��^A��7A��^A�?}A�z�A��7A��uA��uA��!A���A�+A�oA�G�A���A���A���A�ȴA��A���A��9A�~�A�XA���A�$�A�VA}+Au�Ap~�Ao
=Al��Ak�-Aj��Ai�;AiVAf��Ae�AdI�Ab��Aa��A_"�A]/A[ƨAY��AW7LAV-AU��AS�AP�/AN�uAM
=AJ��AI�AG;dAD�RAC��AC�hACG�AB�9ABAAx�AA&�A>��A<�A:��A9
=A8(�A6jA4��A2ĜA1�A1�A17LA0$�A-��A+x�A+oA*�A*�9A*  A'K�A%"�A$�A$v�A!�-A�TA�Al�AȴA�wA�7A^5AXA��AK�A�A(�A��A/AA�A��Az�AC�A�#AS�AȴA��A
�A	G�AVA��A�-A��AK�A 1'@�{@��7@�O�@��`@��@�t�@��\@�$�@�p�@��h@���@���@���@��/@���@��@ꟾ@�V@��@�V@�|�@�P@��@�|�@�S�@�+@��@��y@�ff@�^@�V@��
@��@�~�@�`B@�+@�(�@�@�z�@��y@�%@�9X@�C�@�
=@���@ҟ�@�hs@�I�@�33@Χ�@�5?@�hs@̋D@�r�@�1'@�\)@�n�@Ɂ@ȼj@ǍP@��y@�V@���@�A�@Å@�o@���@°!@�-@�J@���@�V@�bN@���@��!@�-@��^@��D@�I�@���@���@��@�"�@��\@�^5@�{@�x�@���@���@��@�r�@�I�@���@�;d@�33@�
=@���@�ȴ@��R@���@��+@��@��^@�p�@�G�@���@�r�@�(�@��@���@�5?@��@���@��w@�^5@��-@�?}@�%@�X@�@��-@�/@�G�@��7@��@���@���@�O�@��7@�?}@��@��/@�9X@�1@��P@�^5@��H@�+@���@���@���@�t�@�o@�
=@�=q@�@�p�@�X@�&�@�Z@�K�@��R@�5?@�J@��T@��T@��^@�p�@�7L@�&�@���@��@��@��u@��@�Z@�9X@�9X@�1'@�b@�I�@�A�@�|�@��@��!@��+@�M�@���@��T@�$�@��@��7@���@�%@��h@�ff@��\@�M�@��T@�7L@���@�bN@��@�r�@���@�33@���@�J@���@���@��-@�Z@���@�Ĝ@��9@��@��@���@�5?@�5?@�-@�ff@��!@���@�ȴ@��R@��!@���@�v�@�5?@��@��-@��7@�/@�Ĝ@�Z@�  @��@��F@�S�@�K�@�K�@�;d@�o@��y@��!@���@�ff@�-@�@��@��#@��^@��7@�p�@�/@���@��9@�(�@��P@�dZ@��y@�n�@�5?@�@�V@�t�@�ff@�O�@�Z@��@�
=@�ƨ@�(�@��w@�t�@�C�@�+@�C�@�|�@��@�l�@�+@��y@���@�n�@�J@��@���@���@�X@�G�@�G�@�/@�\)@K�@v{@o;d@g
=@Z�\@T�D@J-@B�@<Z@5V@/��@(��@%?}@"�@��@�R@=q@/@	�71111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oB�5B�5B�5B�/B�5B�5B�;B�;B�;B�HB�NB�NB�mB�sB�sB�yB�B�B�B��BB?}B^5BZB/B��B��B�BB�5B�;B�BB�5B�#B��BɺBƨBƨB��B��B��B��B��B��BǮBȴBȴBɺB��B��B��B��B��B��B��B��B��B��B�
B�
B�#B��B��BǮB��B�qB�jB�jB�dB�dB�^B�XB�RB�LB�FB�'B��B��B�bB�%Bu�Be`B\)B`BB^5BL�B2-B�B	7B��B�/BɺB�RB��B�Bn�B`BBP�B?}B/B$�B�B+B
�B
�B
ǮB
�jB
�FB
��B
��B
�+B
l�B
YB
N�B
?}B
!�B
B	��B	ȴB	��B	�-B	�B	��B	��B	��B	�JB	�B	z�B	s�B	hsB	`BB	ZB	W
B	M�B	E�B	A�B	5?B	.B	'�B	!�B	�B	hB	+B��B��B��B��B��B�B�B�B�TB�/B�
B��B��B��B��B�jB�XB�LB�9B�B��B��B��B��B��B��B��B�{B�uB�\B�=B�=B�7B�1B�B�%B�B�B�B�B|�By�Bx�Bv�Bu�Br�Bq�Bp�Bn�Bm�Bm�Bm�Bl�BjBk�BjBhsBe`BcTBbNBaHBcTBgmBgmBgmBgmBgmBe`BffBe`BaHB^5B`BBbNB_;BffBn�Bs�Bw�B{�B�B�=B�VB�VB�VB�VB�\B�VB�\B�\B�bB�oB�uB��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B�B�!B�RB�XB�XB�RB�dB�jB�qBBƨBɺB��B��B��B��B��B�/B�`B�sB�yB�B�B�B�B��B��B��B��B	B	B	JB	PB	VB	VB	VB	hB	bB	\B	VB	JB	
=B	
=B	JB	\B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	 �B	!�B	#�B	,B	1'B	49B	8RB	9XB	8RB	7LB	49B	0!B	/B	0!B	5?B	;dB	A�B	C�B	D�B	G�B	L�B	N�B	M�B	N�B	VB	XB	\)B	^5B	_;B	_;B	aHB	aHB	aHB	hsB	jB	hsB	dZB	bNB	_;B	aHB	ffB	ffB	iyB	l�B	m�B	m�B	n�B	l�B	l�B	n�B	n�B	p�B	r�B	t�B	u�B	w�B	x�B	{�B	{�B	� B	�B	�B	�B	�B	�+B	�1B	�=B	�PB	�bB	�\B	�\B	�hB	�uB	�uB	�{B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�B	�B	�B	�'B	�3B	�9B	�-B	�FB	�LB	�RB	�XB	�RB	�dB	B	ÖB	B	B	�}B	�jB	�jB	�qB	��B	ĜB	ȴB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�
B	�B	�B	�B	�#B	�)B	�)B	�)B	�/B	�/B	�5B	�/B	�/B	�/B	�5B	�5B	�;B	�;B	�BB	�BB	�HB	�HB	�HB	�NB	�ZB	�ZB	�ZB	�ZB	�TB	�HB	�5B	�B	�B	��B	��B	��B	��B	��B	�B	�
B	�B	�B	�B	�)B	�5B	�BB	�HB	�NB	�NB	�NB	�NB	�NB	�TB	�TB	�TB	�`B	�`B	�fB	�mB	�B	��B
B
DB
DB
�B
)�B
33B
:^B
B�B
J�B
Q�B
W
B
ZB
^5B
cTB
iyB
o�B
q�B
v�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111B�5B�5B�5B�/B�5B�5B�;B�;B�;B�HB�NB�NB�mB�sB�sB�yB�B�B�B��B+BC�BbNB_;B2-B��BB�`B�mB�TB�NB�BB�BB�B��BɺBȴB��B��B��B��B��B�B��B��B��B��B��B��B��B��B��B��B��B��B��B��B�B�B�NB�)B�
B��BŢB�}B�jB�jB�dB�jB�^B�XB�RB�RB�RB�dB��B��B�{B�JBy�BhsB\)BaHBcTBR�B7LB"�BVB��B�NB��B�wB��B�1Bs�Be`BVBD�B2-B+B�BhB
��B
�)B
��B
�wB
�jB
�!B
��B
�VB
u�B
]/B
S�B
G�B
1'B

=B	�B	��B	ÖB	�9B	�B	��B	��B	��B	�\B	�+B	}�B	z�B	m�B	dZB	`BB	^5B	P�B	F�B	H�B	;dB	49B	,B	'�B	�B	�B	VB	B��B��B��B��B��B�B�B�sB�ZB�)B�B�#B��BȴB�}B�dB�XB�XB�LB�B��B��B��B��B��B��B��B��B��B�VB�DB�=B�=B�+B�+B�1B�+B�B�B�B}�B{�B{�By�Bt�Bs�Bq�Br�Br�Bo�Bo�Bo�Bo�Bo�Bm�Bk�Bl�Bm�BgmBe`BgmBhsBhsBhsBhsBiyBgmBgmBgmBffBaHBbNBdZBaHBe`Bp�Bu�Bw�B|�B�B�JB�VB�VB�VB�VB�\B�VB�\B�bB�hB�uB��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B�B�B�-B�XB�^B�^B�RB�dB�jB�}BĜBƨBɺB��B��B��B��B�B�5B�fB�sB�yB�B�B�B�B��B��B��B��B	B	+B	JB	VB	VB	VB	\B	oB	bB	bB	\B	PB	DB	
=B	JB	\B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	 �B	 �B	 �B	"�B	$�B	-B	2-B	6FB	9XB	:^B	:^B	9XB	7LB	1'B	0!B	0!B	49B	:^B	B�B	D�B	D�B	G�B	L�B	O�B	M�B	M�B	VB	XB	\)B	^5B	`BB	_;B	bNB	cTB	`BB	hsB	jB	iyB	e`B	dZB	`BB	aHB	gmB	gmB	jB	l�B	m�B	n�B	p�B	m�B	m�B	n�B	n�B	p�B	r�B	t�B	u�B	w�B	y�B	{�B	{�B	� B	�B	�B	�B	�B	�+B	�1B	�=B	�PB	�hB	�bB	�\B	�hB	�uB	�{B	�{B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�B	�B	�B	�-B	�9B	�?B	�3B	�FB	�LB	�RB	�dB	�RB	�^B	B	ÖB	B	ÖB	��B	�jB	�jB	�qB	��B	ĜB	ȴB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�
B	�B	�B	�B	�B	�#B	�)B	�)B	�)B	�/B	�/B	�5B	�/B	�/B	�/B	�5B	�5B	�;B	�;B	�BB	�BB	�HB	�HB	�NB	�TB	�ZB	�`B	�`B	�ZB	�ZB	�NB	�HB	�#B	�B	��B	��B	��B	��B	��B	�
B	�
B	�B	�B	�B	�)B	�5B	�BB	�HB	�NB	�NB	�NB	�NB	�NB	�TB	�TB	�TB	�`B	�`B	�fB	�mB	�B	��B
B
DB
DB
�B
)�B
33B
:^B
B�B
J�B
Q�B
W
B
ZB
^5B
cTB
iyB
n�B
q�B
v�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
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
<T��<#�
<#�
<#�
<#�
<#�
<u<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
PRES            TEMP            PSAL            PRES_ADJUSTED = PRES - dP                                                                                                                                                                                                                                       none                                                                                                                                                                                                                                                            PSAL_ADJUSTED = sw_salt( sw_cndr(PSAL,TEMP,PRES), TEMP, PRES_ADJUSTED ) + CTM                                                                                                                                                                                   dP =-0.1 dbar.                                                                                                                                                                                                                                                  none                                                                                                                                                                                                                                                            CTM: alpha=0.141, tau=6.68s                                                                                                                                                                                                                                     Pressures adjusted by using pressure offset at the sea surface. The quoted error is manufacturer specified accuracy in dbar.                                                                                                                                    The quoted error is manufacturer specified accuracy with respect to ITS-90 at time of laboratory calibration.                                                                                                                                                   No significant salinity drift detected. Salinity adjusted for effects of pressure adjustment. Conductivity cell thermal mass correction (CTM) applied to measurements taken in Cp mode. The quoted error is max[0.01, sqrt(OWerr^2+CTMerr^2)] in PSS-78.        201201031447362012010314473620120103144736  AO  ARGQ                                                                        20111130143139  QCP$                G�O�G�O�G�O�FFBFE           AO  ARGQ                                                                        20111130143139  QCF$                G�O�G�O�G�O�0               UW  ARSQUWQC    WOD & nearby Argo as visual check                               20120103144736  IP                  G�O�G�O�G�O�                