CDF      
      	DATE_TIME         STRING2       STRING4       STRING8       STRING16      STRING32       STRING64   @   	STRING256         N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       D2011-11-30 AOML 2.2 creation; 2015-12-13T22:40:26Z UW 3.1 conversion   
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
_FillValue                    A�   PRES_ADJUSTED            
      	   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     units         decibar    	valid_min                	valid_max         F;�    
resolution        =���   C_format      %7.1f      FORTRAN_format        F7.1f      
_FillValue        G�O�     �  C�   PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                    K�   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     units         decibar    
resolution        =���   C_format      %7.1f      FORTRAN_format        F7.1f      
_FillValue        G�O�     �  M�   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     	valid_min         �      	valid_max         B      
resolution        :�o   C_format      %9.3f      FORTRAN_format        F9.3f      
_FillValue        G�O�     �  U|   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                    ]p   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     	valid_min         �      	valid_max         B      
resolution        :�o   C_format      %9.3f      FORTRAN_format        F9.3f      
_FillValue        G�O�     �  _p   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                    gd   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     units         degree_Celsius     
resolution        :�o   C_format      %9.3f      FORTRAN_format        F9.3f      
_FillValue        G�O�     �  id   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    	valid_min         @      	valid_max         B$     
resolution        :�o   C_format      %9.3f      FORTRAN_format        F9.3f      
_FillValue        G�O�     �  qX   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                    yL   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    	valid_min         @      	valid_max         B$     
resolution        :�o   C_format      %9.3f      FORTRAN_format        F9.3f      
_FillValue        G�O�     �  {L   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                    �@   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     units         psu    
resolution        :�o   C_format      %9.3f      FORTRAN_format        F9.3f      
_FillValue        G�O�     �  �@   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  0  �4   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                    �d   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                    �d   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                    �d   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  ,  �d   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                    ��   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                    ��   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                    ��   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                    ��   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  @  ��   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                    ��   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                    ��   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    ��   HISTORY_START_PRES                    	long_name          Start pressure action applied on   units         decibar    
_FillValue        G�O�        �   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    units         decibar    
_FillValue        G�O�        �   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�        �   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    �Argo profile    3.1 1.2 19500101000000  5901071 US ARGO PROJECT                                                 Stephen Riser                                                   PRES            TEMP            PSAL               ~A   AO  20111130142034  20190522121827  1727_5046_126                   2C  D   APEX                            2143                            040306                          846 @Ը����
1   @Ը�hK��@7�G�z��d�C��1   GPS     Primary sampling: mixed [deeper than nominal 980dbar: discrete; nominal 980dbar to surface: 2dbar-bin averaged]                                                                                                                                                    A   A   A   @�ff@�  A   A   A@  A`  A�  A�  A���A�  A���A�  A�  A�  B   B  B  B  B   B(  B0  B8  B@  BH  BP  BX  B`  Bh  Bp  Bx  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  C �C�C  C  C  C
  C  C  C  C  C  C  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� DfD� D  D� D  D� D  D� D  D� D  D� D  D� D  D�fD  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3y�D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dg��Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do�fDp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dts3Dy�3D�33D�l�D��3D�� D�33D�l�D���D�� D�&fD�Y�D��3D�ٚD�,�D�` Dڰ D��fD�,�D�ffD�fD��11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   @���@�33A��A!��AA��Aa��A���A���A���A���A���A���A���A���B ffBffBffBffB ffB(ffB0ffB8ffB@ffBHffBPffBXffB`ffBhffBpffBxffB�33B�33B�33B�33B�33B�33B�33B�33B�33B�33B�33B�33B�33B�33B�33B�33B�33B�33B�33B�33B�33B�33B�33B�33B�33B�33B�33B�33B�33B�33B�33B�33C 33C33C�C�C�C
�C�C�C�C�C�C�C�C�C�C�C �C"�C$�C&�C(�C*�C,�C.�C0�C2�C4�C6�C8�C:�C<�C>�C@�CB�CD�CF�CH�CJ�CL�CN�CP�CR�CT�CV�CX�CZ�C\�C^�C`�Cb�Cd�Cf�Ch�Cj�Cl�Cn�Cp�Cr�Ct�Cv�Cx�Cz�C|�C~�C��C��C��C��C��C��C��C��C�  C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��D fD �fDfD�fDfD�fDfD�fDfD�fDfD�fDfD�fDfD�fDfD�fD	fD	�fD
fD
�fD�D�fDfD�fDfD�fDfD�fDfD�fDfD�fDfD�fDfD��DfD�fDfD�fDfD�fDfD�fDfD�fDfD�fDfD�fDfD�fDfD�fDfD�fDfD�fDfD�fDfD�fD fD �fD!fD!�fD"fD"�fD#fD#�fD$fD$�fD%fD%�fD&fD&�fD'fD'�fD(fD(�fD)fD)�fD*fD*�fD+fD+�fD,fD,�fD-fD-�fD.fD.�fD/fD/�fD0fD0�fD1fD1�fD2fD2�fD3fD3� D4fD4�fD5fD5�fD6fD6�fD7fD7�fD8fD8�fD9fD9�fD:fD:�fD;fD;�fD<fD<�fD=fD=�fD>fD>�fD?fD?�fD@fD@�fDAfDA�fDBfDB�fDCfDC�fDDfDD�fDEfDE�fDFfDF�fDGfDG�fDHfDH�fDIfDI�fDJfDJ�fDKfDK�fDLfDL�fDMfDM�fDNfDN�fDOfDO�fDPfDP�fDQfDQ�fDRfDR�fDSfDS�fDTfDT�fDUfDU�fDVfDV�fDWfDW�fDXfDX�fDYfDY�fDZfDZ�fD[fD[�fD\fD\�fD]fD]�fD^fD^�fD_fD_�fD`fD`�fDafDa�fDbfDb�fDcfDc�fDdfDd�fDefDe�fDffDf�fDgfDg�fDh  Dh�fDifDi�fDjfDj�fDkfDk�fDlfDl�fDmfDm�fDnfDn�fDofDo��DpfDp�fDqfDq�fDrfDr�fDsfDs�fDtfDty�Dy��D�6fD�p D��fD��3D�6fD�p D�� D��3D�)�D�\�D��fD���D�0 D�c3Dڳ3D��D�0 D�i�D�D��11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   @��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��A�ZA�^5A�^5A�^5A�`BA�`BA�`BA�bNA�dZA�dZA�dZA�dZA�dZA�ffA�ffA�ffA�ffA�hsA�hsA�hsA�jA�jA�jA�jA�jA�l�A�l�A�p�A�r�A�r�A�t�A�t�A�t�A�v�A�x�A�x�A�x�A�z�A�z�A�|�A�~�A��A��A�~�A��A��A��A��A��7A��DA��PA��hA��uA���A���A���A���A���A���A���A���A���A�~�A�`BA�M�A�=qA�-A�&�A��`A�=qA�5?A��A�7LA���A��yA�JA��
A�x�A�"�A���A�XA���A�n�A�;dA��A��A��A��A��uA�x�A�&�A�E�A��A��PA�bA�ƨA��A��A�l�A�VA��yA��A�t�A�33A���A���A�5?A�{A��A���A��+A�=qA���A�^5A���A�33A��HA��A���A�bA�oA�A��RA�  A�x�A��HA�ȴA��^A�XA���A���A�I�A�{A�dZA��RA���A�r�A���A� �A��A���A��9A�"�A�bA�hsA~��A|�Aw"�Arv�Aq�Aol�An�\Aj�Ah��Ag�-AgoAfȴAd�A_/A\bAZ�`AZ��AV�RAR�AQO�AOx�AM��AMVAL�ALn�ALI�AK�AKG�AJ�/AJ�AH��AG�AE��ADffAB�AA�-AA7LA@�9A@5?A?\)A=p�A<5?A;\)A:=qA:$�A:$�A9�A8�`A7�A6��A4�DA3+A2{A0ĜA/|�A-��A,��A+p�A*1A(��A'��A&�jA$ffA#?}A"�A �HA =qA�-A�9AM�A��A��A�A�AZA|�A33AĜA(�A�A�TAhsAĜA�TA�uA��A+An�AZA�A��A��A
��A�RA �A�A\)AM�A&�A�HA�+A33A v�@���@�l�@�
=@�@���@�J@���@�=q@���@�A�@���@�S�@�\@�%@�S�@��@�Z@�J@���@�D@�|�@�^5@ݺ^@۝�@��@�7L@��`@؋D@�C�@֟�@և+@�ff@��T@��@�r�@��@ӕ�@���@���@��@��@ϝ�@���@�?}@���@̴9@�1@�v�@Ɂ@�/@�%@�Ĝ@���@ǍP@�b@��;@�l�@�K�@�+@�"�@�ȴ@��#@Ų-@ř�@��@ċD@�1'@þw@�=q@���@�?}@�ƨ@��@��@���@���@�p�@�&�@��@�r�@��@��@���@���@��!@�E�@���@�hs@�7L@��D@��m@�|�@��+@�-@��@��h@�?}@��`@�V@��@�9X@�z�@�b@�"�@��\@��^@���@��m@�;d@�ȴ@�@���@��@�A�@���@�S�@�"�@�V@���@�/@�Q�@�dZ@��@��H@��R@�n�@�@��j@��@�"�@���@�M�@�v�@�~�@�M�@���@��@�X@�/@�%@��/@��9@��;@�33@�@��@�O�@���@��j@�Z@��@��
@�ƨ@�"�@���@�n�@�$�@�@��@��@��T@��#@��h@�O�@��9@��@�A�@�  @���@�o@�V@��@�p�@�7L@���@��`@�Ĝ@�z�@�(�@���@�ȴ@�$�@��T@���@�p�@�x�@�X@��u@�Q�@�1'@��@�1@��@���@���@���@�~�@�v�@�M�@��@�@���@��h@��@��@�p�@�?}@�&�@�%@��`@��9@�z�@�b@��
@��@���@�V@���@�G�@�&�@���@��`@��`@��`@���@���@�1'@��m@�ƨ@���@��m@��P@�K�@��@���@�M�@�$�@�-@��@��T@� �@�A�@x�`@r^5@e�T@]�@Q��@J^5@Co@=�T@9x�@1%@+ƨ@&��@"�@$�@hs@��@�y@ƨ@|�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   A�ZA�^5A�^5A�^5A�`BA�`BA�`BA�bNA�dZA�dZA�dZA�dZA�dZA�ffA�ffA�ffA�ffA�hsA�hsA�hsA�jA�jA�jA�jA�jA�l�A�l�A�p�A�r�A�r�A�t�A�t�A�t�A�v�A�x�A�x�A�x�A�z�A�z�A�|�A�~�A��A��A�~�A��A��A��A��A��7A��DA��PA��hA��uA���A���A���A���A���A���A���A���A���A�~�A�`BA�M�A�=qA�-A�&�A��`A�=qA�5?A��A�7LA���A��yA�JA��
A�x�A�"�A���A�XA���A�n�A�;dA��A��A��A��A��uA�x�A�&�A�E�A��A��PA�bA�ƨA��A��A�l�A�VA��yA��A�t�A�33A���A���A�5?A�{A��A���A��+A�=qA���A�^5A���A�33A��HA��A���A�bA�oA�A��RA�  A�x�A��HA�ȴA��^A�XA���A���A�I�A�{A�dZA��RA���A�r�A���A� �A��A���A��9A�"�A�bA�hsA~��A|�Aw"�Arv�Aq�Aol�An�\Aj�Ah��Ag�-AgoAfȴAd�A_/A\bAZ�`AZ��AV�RAR�AQO�AOx�AM��AMVAL�ALn�ALI�AK�AKG�AJ�/AJ�AH��AG�AE��ADffAB�AA�-AA7LA@�9A@5?A?\)A=p�A<5?A;\)A:=qA:$�A:$�A9�A8�`A7�A6��A4�DA3+A2{A0ĜA/|�A-��A,��A+p�A*1A(��A'��A&�jA$ffA#?}A"�A �HA =qA�-A�9AM�A��A��A�A�AZA|�A33AĜA(�A�A�TAhsAĜA�TA�uA��A+An�AZA�A��A��A
��A�RA �A�A\)AM�A&�A�HA�+A33A v�@���@�l�@�
=@�@���@�J@���@�=q@���@�A�@���@�S�@�\@�%@�S�@��@�Z@�J@���@�D@�|�@�^5@ݺ^@۝�@��@�7L@��`@؋D@�C�@֟�@և+@�ff@��T@��@�r�@��@ӕ�@���@���@��@��@ϝ�@���@�?}@���@̴9@�1@�v�@Ɂ@�/@�%@�Ĝ@���@ǍP@�b@��;@�l�@�K�@�+@�"�@�ȴ@��#@Ų-@ř�@��@ċD@�1'@þw@�=q@���@�?}@�ƨ@��@��@���@���@�p�@�&�@��@�r�@��@��@���@���@��!@�E�@���@�hs@�7L@��D@��m@�|�@��+@�-@��@��h@�?}@��`@�V@��@�9X@�z�@�b@�"�@��\@��^@���@��m@�;d@�ȴ@�@���@��@�A�@���@�S�@�"�@�V@���@�/@�Q�@�dZ@��@��H@��R@�n�@�@��j@��@�"�@���@�M�@�v�@�~�@�M�@���@��@�X@�/@�%@��/@��9@��;@�33@�@��@�O�@���@��j@�Z@��@��
@�ƨ@�"�@���@�n�@�$�@�@��@��@��T@��#@��h@�O�@��9@��@�A�@�  @���@�o@�V@��@�p�@�7L@���@��`@�Ĝ@�z�@�(�@���@�ȴ@�$�@��T@���@�p�@�x�@�X@��u@�Q�@�1'@��@�1@��@���@���@���@�~�@�v�@�M�@��@�@���@��h@��@��@�p�@�?}@�&�@�%@��`@��9@�z�@�b@��
@��@���@�V@���@�G�@�&�@���@��`@��`@��`@���@���@�1'@��m@�ƨ@���@��m@��P@�K�@��@���@�M�@�$�@�-@��@��T@� �@�A�@x�`@r^5@e�T@]�@Q��@J^5@Co@=�T@9x�@1%@+ƨ@&��@"�@$�@hs@��@�y@ƨ@|�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   ;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oBF�BF�BF�BF�BF�BF�BF�BF�BF�BF�BF�BF�BF�BF�BF�BF�BF�BF�BF�BF�BF�BF�BF�BF�BF�BF�BF�BF�BG�BG�BG�BF�BG�BG�BG�BG�BG�BG�BG�BG�BI�BI�BI�BG�BG�BG�BG�BG�BJ�BL�BM�BO�BQ�BT�BVBXBYBZBZB[#B\)B^5BaHBbNBaHBaHBaHBaHBdZBl�Bv�Br�Bt�Bt�Bq�Bm�Bk�B{�B�7B�DB��B��B�B�-B�?B�FB�-B��B��B��B��BÖB�wB�^B�FB�3B�B�B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B�uB�JB�1B�Bu�BffBT�B<jBhB��B�TB��BĜB�B�B]/BD�B49B�BPBB
��B
�B
�NB
��B
ĜB
�}B
�'B
��B
�\B
aHB
?}B
,B
�B	�B	��B	��B	��B	B	�B	�!B	�B	��B	��B	�B	\)B	G�B	>wB	;dB	�B	
=B	+B��B��B��B�B�B�B�B�B�B�B�fB�TB�NB�HB�HB�BB�;B�5B�/B�B��B��B��B��B��B��B��BƨB��B�qB�XB�3B�!B�B��B��B��B��B�oB�DB�B�B� B� B}�B}�B|�B{�B{�Bz�Bx�Bv�Bu�Bt�Bq�Bq�Bq�Bp�Bp�Bp�Bo�Bn�Bm�BjBhsBffBdZB`BB]/BYBXBVBS�BT�BO�BN�BM�BL�BL�BK�BH�BH�BH�BH�BG�BF�BE�BD�BC�BH�BI�BP�BR�BR�BR�BS�BQ�BR�BP�BL�BJ�BR�BS�BN�BL�BJ�BJ�BM�BM�BM�BL�BL�BM�BM�BN�BN�BP�BR�BT�BT�BS�BT�BW
BXBXBYB]/B]/B]/B\)B]/B^5B^5B^5B`BBiyBu�Bz�B|�B~�B�B�B�B� B�B�%B�+B�7B�7B�=B�7B�=B�JB�JB�bB�oB��B��B��B��B��B��B��B��B��B��B�B�B�B�-B�9B�LB�^B�dB�qB�}B��BBĜBĜBƨBɺB��B��B�)B�;B�HB�fB�B�B��B��B��B��B	B	%B	%B	1B	
=B	DB	bB	hB	uB	�B	�B	�B	�B	�B	�B	�B	�B	#�B	$�B	%�B	%�B	'�B	(�B	+B	-B	-B	.B	.B	/B	/B	/B	33B	6FB	>wB	A�B	B�B	C�B	C�B	D�B	G�B	H�B	H�B	M�B	Q�B	R�B	VB	W
B	YB	YB	YB	YB	\)B	^5B	aHB	e`B	hsB	iyB	iyB	m�B	t�B	v�B	x�B	z�B	}�B	� B	�B	�B	�+B	�7B	�7B	�7B	�7B	�DB	�DB	�VB	�hB	�{B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�B	�!B	�'B	�'B	�-B	�3B	�?B	�LB	�^B	�^B	�wB	��B	��B	��B	B	ÖB	ÖB	ÖB	ÖB	ÖB	ÖB	ĜB	ƨB	ȴB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�/B	�B	��B
B
VB
�B
�B
%�B
2-B
8RB
>wB
F�B
L�B
O�B
VB
\)B
aHB
gmB
l�B
p�B
u�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   BF�BF�BF�BF�BF�BF�BF�BF�BF�BF�BF�BF�BF�BF�BF�BF�BF�BF�BF�BF�BF�BF�BF�BF�BF�BF�BF�BF�BG�BG�BG�BF�BG�BG�BG�BG�BG�BG�BG�BG�BI�BI�BI�BG�BG�BG�BG�BG�BJ�BL�BM�BO�BQ�BT�BVBXBYBZBZB[#B\)B_;BaHBbNBaHBaHBaHBbNBffBo�Bv�Bx�Bz�B{�Bu�Bs�Br�B�B�DB�VB��B��B�B�3B�LB�^B�9B��B��B�B��BĜB��B�jB�LB�FB�'B�B�B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B�\B�=B�1Bx�BiyBZBF�B�B��B�fB�
B��B�LB�\BffBI�B<jB%�BhB+B
��B
�B
�B
��B
ŢB
ŢB
�FB
��B
��B
l�B
C�B
0!B
(�B	��B	�B	��B	��B	��B	�-B	�3B	�!B	��B	��B	�VB	cTB	J�B	?}B	E�B	(�B	\B	JB	B��B��B�B�B�B�B�B�B�B�B�mB�mB�fB�ZB�HB�BB�;B�;B�5B�B��B��B��B��B��B��B��BÖBÖB�qB�FB�9B�!B�B��B��B��B��B�VB�7B�1B�B�B�B� B~�B~�B|�B|�B{�Bz�Bv�Bv�Bt�Br�Bs�Br�Bq�Bp�Bq�Bp�Bp�Bn�Bk�BhsBgmBgmBe`B]/BZBZBXBW
BS�BQ�BP�BO�BM�BL�BK�BJ�BJ�BH�BH�BH�BG�BH�BG�BI�BK�BQ�BS�BS�BS�BVBT�BT�BS�BO�BL�BS�BVBW
BS�BN�BM�BN�BN�BN�BN�BL�BM�BM�BO�BO�BQ�BS�BVBW
BW
BW
BW
BYBZB[#B]/B]/B^5B^5B^5B_;B^5B^5BbNBhsBt�Bz�B}�B~�B�B�B�B�B�B�%B�1B�=B�=B�DB�DB�DB�PB�VB�hB�{B��B��B��B��B��B��B��B��B��B��B�B�B�!B�-B�9B�RB�dB�jB�}B��B��BÖBŢBŢBƨBɺB��B��B�/B�HB�NB�sB�B�B��B��B��B	  B	B	+B	+B		7B	
=B	JB	hB	oB	{B	�B	�B	�B	�B	�B	�B	�B	 �B	$�B	%�B	%�B	%�B	'�B	(�B	,B	-B	-B	.B	.B	/B	/B	0!B	49B	8RB	?}B	A�B	C�B	C�B	D�B	E�B	G�B	H�B	I�B	N�B	Q�B	S�B	VB	W
B	YB	YB	YB	YB	]/B	_;B	bNB	e`B	hsB	jB	jB	n�B	u�B	w�B	x�B	z�B	}�B	� B	�B	�B	�1B	�=B	�=B	�=B	�7B	�DB	�DB	�VB	�oB	�{B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�B	�B	�!B	�'B	�'B	�-B	�3B	�?B	�RB	�^B	�jB	�wB	��B	��B	B	B	ÖB	ÖB	ÖB	ÖB	ÖB	ÖB	ŢB	ǮB	ȴB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�/B	�B	��B
B
VB
�B
�B
%�B
2-B
7LB
>wB
F�B
L�B
O�B
VB
\)B
aHB
gmB
l�B
p�B
u�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   <#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
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
<T��<49X<#�
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
PRES            TEMP            PSAL            PRES_ADJUSTED = PRES - dP                                                                                                                                                                                                                                       none                                                                                                                                                                                                                                                            PSAL_ADJUSTED = sw_salt( sw_cndr(PSAL,TEMP,PRES), TEMP, PRES_ADJUSTED ) + CTM                                                                                                                                                                                   dP =-0.1 dbar.                                                                                                                                                                                                                                                  none                                                                                                                                                                                                                                                            CTM: alpha=0.141, tau=6.68s                                                                                                                                                                                                                                     Pressures adjusted by using pressure offset at the sea surface. The quoted error is manufacturer specified accuracy in dbar.                                                                                                                                    The quoted error is manufacturer specified accuracy with respect to ITS-90 at time of laboratory calibration.                                                                                                                                                   No significant salinity drift detected. Salinity adjusted for effects of pressure adjustment. Conductivity cell thermal mass correction (CTM) applied to measurements taken in Cp mode. The quoted error is max[0.01, sqrt(OWerr^2+CTMerr^2)] in PSS-78.        201201031447182012010314471820120103144718  AO  ARGQ                                                                        20111130142034  QCP$                G�O�G�O�G�O�FFBFE           AO  ARGQ                                                                        20111130142034  QCF$                G�O�G�O�G�O�0               UW  ARSQUWQC    WOD & nearby Argo as visual check                               20120103144718  IP                  G�O�G�O�G�O�                