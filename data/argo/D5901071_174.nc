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
_FillValue                    ��Argo profile    3.1 1.2 19500101000000  5901071 US ARGO PROJECT                                                 Stephen Riser                                                   PRES            TEMP            PSAL               �A   AO  20111130143109  20190522121827  1727_5046_174                   2C  D   APEX                            2143                            040306                          846 @�������1   @����-�@5�V��dQ��1   GPS     Primary sampling: mixed [deeper than nominal 980dbar: discrete; nominal 980dbar to surface: 2dbar-bin averaged]                                                                                                                                                    A   A   A   @�ff@�  A   A   A@  A`  A�  A�33A�  A�  A�  A�  A�  A�  B   B  B  B  B   B(ffB0ffB8ffB@  BH  BP  BW��B_��Bh  Bp  Bx  B�  B�ffB���B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  C   C  C  C  C  C
  C  C  C  C  C�C  C�fC  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C�  C�  C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D��D� D  D� D  D� D	  D	� D
  D
� D  D�fDfD� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  Dy�D  D� D  D� D  D� D  D� D  D� D  D� D  D� D��D� D fD � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Ds��Dy��D�33D�y�D�� D��fD�)�D�l�D���D��fD�6fD�` D��3D�� D�,�D�i�Dڃ3D��fD�3D�Y�D� D��1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111@���@�33A��A!��AA��Aa��A���A�  A���A���A���A���A���A���B ffBffBffBffB ffB(��B0��B8��B@ffBHffBPffBX  B`  BhffBpffBxffB�33B���B�  B�33B�33B�33B�33B�33B�33B�33B�33B�33B�33B�33B�33B�33B�33B�33B�33B�33B�33B�33B�33B�33B�33B�33B�33B�33B�33B�33B�33B�33C �C�C�C�C�C
�C�C�C�C�C33C�C  C�C�C�C �C"�C$�C&�C(�C*�C,�C.�C0�C2�C4�C6�C8�C:�C<�C>�C@�CB�CD�CF�CH�CJ�CL�CN�CP�CR�CT�CV�CX�CZ�C\�C^�C`�Cb�Cd�Cf�Ch�Cj�Cl�Cn�Cp�Cr�Ct�Cv�Cx�Cz�C|�C~�C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C�  C��C��C�  C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C�  C��C��C��C��C��C��C��D fD �fDfD�fDfD�fDfD�fDfD�fDfD�fD  D�fDfD�fDfD�fD	fD	�fD
fD
�fDfD��D�D�fDfD�fDfD�fDfD�fDfD�fDfD�fDfD�fDfD�fDfD�fDfD�fDfD�fDfD� DfD�fDfD�fDfD�fDfD�fDfD�fDfD�fDfD�fD  D�fD �D �fD!fD!�fD"fD"�fD#fD#�fD$fD$�fD%fD%�fD&fD&�fD'fD'�fD(fD(�fD)fD)�fD*fD*�fD+fD+�fD,fD,�fD-fD-�fD.fD.�fD/fD/�fD0fD0�fD1fD1�fD2fD2�fD3fD3�fD4fD4�fD5fD5�fD6fD6�fD7fD7�fD8fD8�fD9fD9�fD:fD:�fD;fD;�fD<fD<�fD=fD=�fD>fD>�fD?fD?�fD@fD@�fDAfDA�fDBfDB�fDCfDC�fDDfDD�fDEfDE�fDFfDF�fDGfDG�fDHfDH�fDIfDI�fDJfDJ�fDKfDK�fDLfDL�fDMfDM�fDNfDN�fDOfDO�fDPfDP�fDQfDQ�fDRfDR�fDSfDS�fDTfDT�fDUfDU�fDVfDV�fDWfDW�fDXfDX�fDYfDY�fDZfDZ�fD[fD[�fD\fD\�fD]fD]�fD^fD^�fD_fD_�fD`fD`�fDafDa�fDbfDb�fDcfDc�fDdfDd�fDefDe�fDffDf�fDgfDg�fDhfDh�fDifDi�fDjfDj�fDkfDk�fDlfDl�fDmfDm�fDnfDn�fDofDo�fDpfDp�fDqfDq�fDrfDr�fDsfDs�fDt  Dy� D�6fD�|�D��3D���D�,�D�p D�� D��D�9�D�c3D��fD��3D�0 D�l�DچfD��D�fD�\�D�3D� 1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��AլA՝�AՓuAՉ7A�|�A�t�AՁA�|�AՍPA՗�AՋDA�jA�;dA��A���Aԝ�A�bNA�bA���A�Q�A�$�A��A��mA�9XA�p�AυA�XA�p�AƋDA�\)A�A�A��mA��
A�9XA��!A�z�A��A���A�^5A��A�t�A�ZA��;A��7A��+A�"�A�v�A�l�A�A���A��jA�l�A���A�;dA�`BA��A�ffA��#A��PA�bNA�JA�ĜA�?}A�A��A���A��PA��wA��RA�VA�t�A��DA���A�dZA��jA���A�ƨA��A�E�A��yA���A��A�JA���A�ƨA��A�\)A��FA�\)A�&�A��A��A��PA�/A��#A�jA���A�p�A��HA�G�A���A�dZA�\)A�VA��+A��TA��uA��;A���A�E�A~��A}G�A|�!A|z�A|  A{VAy��Ax�`Aw?}AuAt1Ap�jAmhsAkp�Ai��Ag�wAfbAe%Ad�!Ac�Aa;dA_x�A\�AZ~�AXVAW/AU��AT�yAR�\APz�AO�AM��ALZAJ�AI%AG��AG&�AF{AD�/ABZA?�PA?�A>�/A>I�A=��A=dZA=A;�A9�A9x�A9"�A8�9A7�A7O�A6��A6A5�-A3��A2�A1�A0�A0{A/C�A-�;A,5?A*�\A(��A'��A'�hA'XA'VA&ĜA&(�A%7LA$ZA#l�A"�A ��AƨA��AXA~�AZA�A��A��A��AA�AdZA��A�A"�AAG�A��A^5A�A��Al�AoA�A�yAAoA
 �A	"�A��A��A�A�uA�DA�AjA �A�AC�A�jA Ĝ@�x�@��
@�C�@��9@��7@��@��@��
@�\@�j@�bN@�ƨ@��@�7L@�D@��@䛦@�"�@���@�bN@߅@�l�@�S�@��@�ff@�$�@�&�@ۮ@��H@ٺ^@�%@�r�@�I�@�b@ץ�@�\)@��H@�~�@��@Լj@�%@�p�@Ցh@ղ-@�5?@�Ĝ@�I�@�dZ@��@͡�@�&�@�%@�Ĝ@��@�G�@�/@�%@�bN@�@�`B@ă@�A�@��;@ÍP@���@�j@��@�v�@��@���@�V@�Z@���@�t�@���@�n�@�{@��^@�O�@��@���@�z�@�Z@��@��
@�dZ@��@�
=@�@�ȴ@�^5@��-@�%@��u@���@���@���@�$�@��^@��h@�p�@�Q�@���@���@��@�|�@�@��@��\@��@�@���@��@��@��9@��u@��u@��@��@�(�@��H@���@��\@�^5@�V@�M�@��@�p�@�&�@�%@��`@��j@��9@��@��D@�Q�@�1'@��@�1@��w@��H@�$�@���@��@�bN@��
@��@���@��@�t�@�l�@�dZ@�\)@�C�@�
=@���@���@��+@�~�@�~�@�v�@�V@��@�hs@���@���@�r�@�A�@���@�ƨ@�ƨ@��@�\)@�ȴ@�{@��^@��h@���@��7@�/@��@���@��j@�Z@�t�@���@�$�@��@�X@�V@��j@���@��D@�j@��@��@�|�@�"�@��\@�=q@��@�J@��^@�`B@�?}@�&�@��@���@�j@��
@���@�|�@�\)@�;d@��@���@��!@�v�@��@���@��@���@��`@�Ĝ@��9@��u@�  @���@���@�33@�+@�@���@���@��\@�^5@�-@�$�@��@�J@��-@�X@��@�Z@��P@�\)@��y@��@���@�=q@���@���@���@�p�@�`B@�?}@��@�Q�@�(�@��@�V@u�-@o|�@fȴ@ZM�@T�@O�@A�#@;C�@7�P@1hs@-�-@(r�@$1@t�@�P@�\@
=@C�@�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111AլA՝�AՓuAՉ7A�|�A�t�AՁA�|�AՍPA՗�AՋDA�jA�;dA��A���Aԝ�A�bNA�bA���A�Q�A�$�A��A��mA�9XA�p�AυA�XA�p�AƋDA�\)A�A�A��mA��
A�9XA��!A�z�A��A���A�^5A��A�t�A�ZA��;A��7A��+A�"�A�v�A�l�A�A���A��jA�l�A���A�;dA�`BA��A�ffA��#A��PA�bNA�JA�ĜA�?}A�A��A���A��PA��wA��RA�VA�t�A��DA���A�dZA��jA���A�ƨA��A�E�A��yA���A��A�JA���A�ƨA��A�\)A��FA�\)A�&�A��A��A��PA�/A��#A�jA���A�p�A��HA�G�A���A�dZA�\)A�VA��+A��TA��uA��;A���A�E�A~��A}G�A|�!A|z�A|  A{VAy��Ax�`Aw?}AuAt1Ap�jAmhsAkp�Ai��Ag�wAfbAe%Ad�!Ac�Aa;dA_x�A\�AZ~�AXVAW/AU��AT�yAR�\APz�AO�AM��ALZAJ�AI%AG��AG&�AF{AD�/ABZA?�PA?�A>�/A>I�A=��A=dZA=A;�A9�A9x�A9"�A8�9A7�A7O�A6��A6A5�-A3��A2�A1�A0�A0{A/C�A-�;A,5?A*�\A(��A'��A'�hA'XA'VA&ĜA&(�A%7LA$ZA#l�A"�A ��AƨA��AXA~�AZA�A��A��A��AA�AdZA��A�A"�AAG�A��A^5A�A��Al�AoA�A�yAAoA
 �A	"�A��A��A�A�uA�DA�AjA �A�AC�A�jA Ĝ@�x�@��
@�C�@��9@��7@��@��@��
@�\@�j@�bN@�ƨ@��@�7L@�D@��@䛦@�"�@���@�bN@߅@�l�@�S�@��@�ff@�$�@�&�@ۮ@��H@ٺ^@�%@�r�@�I�@�b@ץ�@�\)@��H@�~�@��@Լj@�%@�p�@Ցh@ղ-@�5?@�Ĝ@�I�@�dZ@��@͡�@�&�@�%@�Ĝ@��@�G�@�/@�%@�bN@�@�`B@ă@�A�@��;@ÍP@���@�j@��@�v�@��@���@�V@�Z@���@�t�@���@�n�@�{@��^@�O�@��@���@�z�@�Z@��@��
@�dZ@��@�
=@�@�ȴ@�^5@��-@�%@��u@���@���@���@�$�@��^@��h@�p�@�Q�@���@���@��@�|�@�@��@��\@��@�@���@��@��@��9@��u@��u@��@��@�(�@��H@���@��\@�^5@�V@�M�@��@�p�@�&�@�%@��`@��j@��9@��@��D@�Q�@�1'@��@�1@��w@��H@�$�@���@��@�bN@��
@��@���@��@�t�@�l�@�dZ@�\)@�C�@�
=@���@���@��+@�~�@�~�@�v�@�V@��@�hs@���@���@�r�@�A�@���@�ƨ@�ƨ@��@�\)@�ȴ@�{@��^@��h@���@��7@�/@��@���@��j@�Z@�t�@���@�$�@��@�X@�V@��j@���@��D@�j@��@��@�|�@�"�@��\@�=q@��@�J@��^@�`B@�?}@�&�@��@���@�j@��
@���@�|�@�\)@�;d@��@���@��!@�v�@��@���@��@���@��`@�Ĝ@��9@��u@�  @���@���@�33@�+@�@���@���@��\@�^5@�-@�$�@��@�J@��-@�X@��@�Z@��P@�\)@��y@��@���@�=q@���@���@���@�p�@�`B@�?}@��@�Q�@�(�@��@�V@u�-@o|�@fȴ@ZM�@T�@O�@A�#@;C�@7�P@1hs@-�-@(r�@$1@t�@�P@�\@
=@C�@�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oBw�Bx�Bx�Bx�Bz�B� B�B�B�DB�VB�VB�7B�B�B� Bz�Bw�Bx�By�Bx�Bx�B|�B�DB�PB�Bl�BdZB`BBVBP�BN�BI�B@�B]/Bp�BcTBu�B��B��B��B��B�?B�RB�XB�wB��B��BȴBȴBɺB��BȴBŢBB�}B�^B�-B�B��B��B��B��B��B�Bs�Bq�Bn�BffBZBP�BI�B>wB2-B%�B�B\B%BB��B��B�fB��B��B�LB��B��B��B�PBx�BhsB_;BVBO�BJ�BF�B?}B7LB/B%�B�B{BJB
��B
�B
�HB
��B
�3B
� B
jB
N�B
@�B
:^B
7LB
49B
0!B
(�B
 �B
�B
PB
B	��B	�)B	ƨB	�RB	�B	��B	��B	�{B	�oB	�DB	�B	|�B	l�B	[#B	N�B	G�B	A�B	<jB	2-B	�B	hB	JB	  B��B�B��B	+B	VB��B�fB��B��B��B��B��B��B��BɺB��B��B��B��B��B��BȴBȴBƨBŢBŢBĜBÖB��B�jB�RB�LB�B�B�B��B��B��B��B��B��B��B�{B�hB�JB�7B�B� B~�B}�Bz�Bu�Bv�B{�B|�Bz�Bx�Bv�Bv�Bv�Bv�Bt�Bt�Bt�Bs�Br�Bq�Bo�Br�Br�Bq�Bq�Bs�Bs�Bs�Br�Br�Br�Bq�Bp�Bo�Bm�Bl�Bk�BiyBjBjBhsBgmBjBiyBjBjBjBk�Bk�BjBiyBjBjBjBl�Bn�Bn�Bp�Br�Br�Bq�Bq�Br�Br�Bs�Bw�Bx�By�By�Bz�Bz�Bz�B{�B{�B|�B}�B}�B�%B�PB�hB�oB�hB�PB�DB�JB�=B�JB��B��B��B��B�B�3B�9B�9B�FB�qBÖBŢBƨBȴBȴB��B�B�/B�HB�TB�`B�mB�yB�B�B�B�B�B�B�B��B��B��B��B��B	B	+B		7B	
=B	
=B	DB	PB	bB	{B	�B	�B	�B	�B	"�B	%�B	&�B	&�B	.B	0!B	1'B	2-B	2-B	5?B	5?B	6FB	9XB	:^B	<jB	<jB	<jB	<jB	>wB	?}B	@�B	?}B	@�B	H�B	K�B	O�B	R�B	S�B	W
B	XB	\)B	_;B	`BB	bNB	dZB	e`B	e`B	gmB	iyB	k�B	k�B	k�B	l�B	r�B	v�B	y�B	|�B	~�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�%B	�%B	�+B	�+B	�+B	�+B	�1B	�=B	�JB	�\B	�bB	�hB	�oB	�uB	�{B	�{B	�{B	�{B	�{B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�B	�B	�B	�B	�!B	�!B	�'B	�3B	�9B	�?B	�?B	�LB	�XB	�^B	�^B	�^B	�^B	�dB	�wB	�}B	�}B	��B	��B	��B	��B	B	B	ÖB	ĜB	ƨB	ƨB	ƨB	ǮB	ǮB	ǮB	ɺB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�B	�B	�/B	�5B	�fB	��B
B
1B
bB
�B
'�B
(�B
9XB
B�B
G�B
L�B
O�B
VB
\)B
dZB
hsB
n�B
q�B
u�B
z�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111Bw�Bx�Bx�Bx�Bz�B� B�B�B�DB�VB�\B�=B�B�B�B{�By�By�B|�By�Bx�B}�B�PB�\B�1Bv�Bq�BdZBW
BP�BO�BK�BC�B`BBx�Br�Bz�B��B��B��B��B�LB�^B�wB��BĜBǮB��BɺB��B��B��BɺBǮBĜB��B�?B�B�B��B��B��B��B�PBt�Br�Br�Bk�B]/BS�BN�BB�B6FB'�B�BoB+BB��B��B�B�B��B�jB�B��B��B�{B~�Bk�BbNBXBQ�BK�BH�BA�B9XB2-B(�B�B�BhBB
�B
�`B
�BB
ǮB
�+B
x�B
T�B
D�B
<jB
8RB
6FB
33B
,B
$�B
�B
oB
1B
  B	�fB	��B	�qB	�-B	��B	��B	��B	��B	�bB	�7B	�B	r�B	`BB	Q�B	J�B	D�B	C�B	7LB	�B	�B	bB	B��B��B��B	DB	oB	%B�B�B��B��B��B��B��B��B��B��B��B��B��B��B��B��BɺB��B��BȴBŢBŢBÖB��B�qB�jB�?B�B�B�B��B��B��B��B��B��B��B�{B�\B�JB�%B�B� B� B�Bx�Bx�B�B� B}�Bz�By�Bz�By�Bw�Bv�Bv�Bu�Bt�Bs�Bs�Bu�Bv�Bu�Bu�Bu�Bt�Bs�Bs�Br�Br�Br�Br�Br�Bs�Bs�Bt�Bp�Bn�Bl�Bk�Bk�Bk�Bl�Bl�Bk�Bl�Bl�Bl�Bl�Bk�Bk�Bk�Bl�Bm�Bn�Bp�Bp�Bq�Br�Br�Br�Br�Bs�Bt�Bv�Bx�Bz�Bz�Bz�Bz�B{�B{�B|�B|�B}�B~�B}�B�%B�JB�hB�oB��B�bB�JB�VB�DB�VB��B��B��B��B�B�3B�9B�?B�RB�}BĜBŢBǮBȴB��B��B�#B�5B�NB�ZB�fB�sB�B�B�B�B�B�B�B�B��B��B��B��B	  B	B	1B		7B	
=B	
=B	JB	PB	hB	�B	�B	�B	�B	 �B	#�B	%�B	'�B	(�B	/B	0!B	2-B	2-B	33B	5?B	6FB	7LB	:^B	:^B	<jB	=qB	<jB	<jB	>wB	?}B	@�B	@�B	B�B	H�B	K�B	O�B	R�B	S�B	W
B	YB	\)B	_;B	`BB	bNB	dZB	e`B	e`B	gmB	iyB	k�B	k�B	k�B	m�B	s�B	w�B	z�B	}�B	� B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�%B	�%B	�+B	�+B	�+B	�+B	�7B	�DB	�PB	�\B	�bB	�hB	�uB	�uB	�{B	�{B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�B	�B	�B	�!B	�!B	�'B	�-B	�9B	�9B	�?B	�FB	�RB	�XB	�^B	�^B	�^B	�dB	�jB	�wB	�}B	�}B	��B	��B	��B	B	B	ÖB	ĜB	ŢB	ƨB	ƨB	ƨB	ǮB	ǮB	ȴB	ɺB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�B	�#B	�/B	�5B	�fB	��B
B
1B
bB
�B
'�B
(�B
9XB
B�B
G�B
L�B
O�B
VB
\)B
dZB
hsB
n�B
q�B
u�B
z�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<49X<T��<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<�o<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<D��<��
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
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
PRES            TEMP            PSAL            PRES_ADJUSTED = PRES - dP                                                                                                                                                                                                                                       none                                                                                                                                                                                                                                                            PSAL_ADJUSTED = sw_salt( sw_cndr(PSAL,TEMP,PRES), TEMP, PRES_ADJUSTED ) + CTM                                                                                                                                                                                   dP =-0.1 dbar.                                                                                                                                                                                                                                                  none                                                                                                                                                                                                                                                            CTM: alpha=0.141, tau=6.68s                                                                                                                                                                                                                                     Pressures adjusted by using pressure offset at the sea surface. The quoted error is manufacturer specified accuracy in dbar.                                                                                                                                    The quoted error is manufacturer specified accuracy with respect to ITS-90 at time of laboratory calibration.                                                                                                                                                   No significant salinity drift detected. Salinity adjusted for effects of pressure adjustment. Conductivity cell thermal mass correction (CTM) applied to measurements taken in Cp mode. The quoted error is max[0.01, sqrt(OWerr^2+CTMerr^2)] in PSS-78.        201201031447352012010314473520120103144735  AO  ARGQ                                                                        20111130143109  QCP$                G�O�G�O�G�O�FFBFE           AO  ARGQ                                                                        20111130143109  QCF$                G�O�G�O�G�O�0               UW  ARSQUWQC    WOD & nearby Argo as visual check                               20120103144735  IP                  G�O�G�O�G�O�                