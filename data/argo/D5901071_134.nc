CDF      
      	DATE_TIME         STRING2       STRING4       STRING8       STRING16      STRING32       STRING64   @   	STRING256         N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       D2011-11-30 AOML 2.2 creation; 2015-12-13T22:40:28Z UW 3.1 conversion   
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
_FillValue                 �  A<   PRES_ADJUSTED            
      	   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     units         decibar    	valid_min                	valid_max         F;�    
resolution        =���   C_format      %7.1f      FORTRAN_format        F7.1f      
_FillValue        G�O�     �  C$   PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  J�   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     units         decibar    
resolution        =���   C_format      %7.1f      FORTRAN_format        F7.1f      
_FillValue        G�O�     �  L�   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     	valid_min         �      	valid_max         B      
resolution        :�o   C_format      %9.3f      FORTRAN_format        F9.3f      
_FillValue        G�O�     �  TD   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  [�   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     	valid_min         �      	valid_max         B      
resolution        :�o   C_format      %9.3f      FORTRAN_format        F9.3f      
_FillValue        G�O�     �  ]�   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ed   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     units         degree_Celsius     
resolution        :�o   C_format      %9.3f      FORTRAN_format        F9.3f      
_FillValue        G�O�     �  gL   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    	valid_min         @      	valid_max         B$     
resolution        :�o   C_format      %9.3f      FORTRAN_format        F9.3f      
_FillValue        G�O�     �  n�   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  v�   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    	valid_min         @      	valid_max         B$     
resolution        :�o   C_format      %9.3f      FORTRAN_format        F9.3f      
_FillValue        G�O�     �  xl   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     units         psu    
resolution        :�o   C_format      %9.3f      FORTRAN_format        F9.3f      
_FillValue        G�O�     �  ��   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  0  ��   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                    ��   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                    ��   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                    ��   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  ,  ��   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                    ��   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                    ��   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                    ��   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                    ��   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  @  ��   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                    �8   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                    �H   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    �L   HISTORY_START_PRES                    	long_name          Start pressure action applied on   units         decibar    
_FillValue        G�O�        �\   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    units         decibar    
_FillValue        G�O�        �`   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�        �d   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    �hArgo profile    3.1 1.2 19500101000000  5901071 US ARGO PROJECT                                                 Stephen Riser                                                   PRES            TEMP            PSAL               �A   AO  20111130142221  20190522121827  1727_5046_134                   2C  D   APEX                            2143                            040306                          846 @���S�1   @����Q�@7O��v��d�`A�71   GPS     Primary sampling: mixed [deeper than nominal 980dbar: discrete; nominal 980dbar to surface: 2dbar-bin averaged]                                                                                                                                                    A   A   A   @�ff@���A   A   A@  A`  A�  A�  A�  A�  A�  A�  A�  A�  B   B  B  B  B��B(  B0  B8  B@  BH  BP  BX  B`  Bh  Bp  Bx  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  C   C  C  C  C  C
  C  C  C  C  C  C  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� DfD� D  D� D  D� D  D� D  D� D  D� D  D� D  Dy�D  D� D  D� D  D� D  D� D   D �fD!fD!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+y�D+��D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP�fDQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn�fDofDo�fDp  Dp� Dq  Dq� Dr  Dr� Ds  Ds�fDy��1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 @���@�  A��A!��AA��Aa��A���A���A���A���A���A���A���A���B ffBffBffBffB   B(ffB0ffB8ffB@ffBHffBPffBXffB`ffBhffBpffBxffB�33B�33B�33B�33B�33B�33B�33B�33B�33B�33B�33B�33B�33B�33B�33B�33B�33B�33B�33B�33B�33B�33B�33B�33B�33B�33B�33B�33B�33B�33B�33B�33C �C�C�C�C�C
�C�C�C�C�C�C�C�C�C�C�C �C"�C$�C&�C(�C*�C,�C.�C0�C2�C4�C6�C8�C:�C<�C>�C@�CB�CD�CF�CH�CJ�CL�CN�CP�CR�CT�CV�CX�CZ�C\�C^�C`�Cb�Cd�Cf�Ch�Cj�Cl�Cn�Cp�Cr�Ct�Cv�Cx�Cz�C|�C~�C��C��C��C��C��C��C��C��C��C�  C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C�  C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��D fD �fDfD�fDfD�fDfD�fDfD�fDfD�fDfD�fDfD�fDfD�fD	fD	�fD
fD
�fDfD�fDfD�fDfD�fDfD�fDfD�fDfD�fDfD�fDfD�fDfD�fD�D�fDfD�fDfD�fDfD�fDfD�fDfD�fDfD�fDfD� DfD�fDfD�fDfD�fDfD�fD fD ��D!�D!�fD"fD"�fD#fD#�fD$fD$�fD%fD%�fD&fD&�fD'fD'�fD(fD(�fD)fD)�fD*fD*�fD+fD+� D,  D,�fD-fD-�fD.fD.�fD/fD/�fD0fD0�fD1fD1�fD2fD2�fD3fD3�fD4fD4�fD5fD5�fD6fD6�fD7fD7�fD8fD8�fD9fD9�fD:fD:�fD;fD;�fD<fD<�fD=fD=�fD>fD>�fD?fD?�fD@fD@�fDAfDA�fDBfDB�fDCfDC�fDDfDD�fDEfDE�fDFfDF�fDGfDG�fDHfDH�fDIfDI�fDJfDJ�fDKfDK�fDLfDL�fDMfDM�fDNfDN�fDOfDO�fDPfDP��DQfDQ�fDRfDR�fDSfDS�fDTfDT�fDUfDU�fDVfDV�fDWfDW�fDXfDX�fDYfDY�fDZfDZ�fD[fD[�fD\fD\�fD]fD]�fD^fD^�fD_fD_�fD`fD`�fDafDa�fDbfDb�fDcfDc�fDdfDd�fDefDe�fDffDf�fDgfDg�fDhfDh�fDifDi�fDjfDj�fDkfDk�fDlfDl�fDmfDm�fDnfDn��Do�Do��DpfDp�fDqfDq�fDrfDr�fDsfDs��Dy�31111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 @��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��A�9XA�;dA�A�A�C�A�K�A�K�A�M�A�I�A�M�A�M�A�E�A�C�A�9XA�=qA��A��9A�n�A�t�A��^A�&�A���A�ƨA��jA��PA�M�A�?}A�"�A�{A��A�ȴA�\)A���A��;A�M�A�`BA�+A��A�C�A��+A��-A�?}A��jA��\A�z�A�^5A�?}A��A��A��!A���A���A��A�^5A�K�A�1'A���A�+A��9A��A��A�(�A��hA�O�A��mA��A�x�A�I�A�=qA�bA�\)A��+A��uA�`BA��A��!A��A��^A�I�A���A��TA��#A��uA��+A�ffA�-A�t�A���A�z�A���A���A��A���A��A��A��HA�ZA�
=A��A��7A���A���A�?}A��A��A���A��A�r�A��FA�  A�r�A��A�  A� �A� �A�ȴA�^5A��A|�A~ȴA~�jA~jA}��A|�\A{�;A{�hAz�AyK�AxI�Aw��AvVAu\)At��As|�Ar�Aq
=Ao?}An�AnbNAm�Ak&�AjZAiG�AgAdn�Ab�\Aa"�A_�wA_?}A]��A\(�A[�wAZA�AX��AWXAVbNAQ�;AN��AM�AL(�AK|�AJ��AJJAIG�AH�yAHQ�AF�AFE�AE�AC7LAAAA;dA?�A=�A<A�A9XA8�+A7K�A5��A5?}A5`BA5\)A5A4I�A2�A0  A-hsA+��A*��A)�hA)/A(�!A({A'XA&z�A&�A%/A$�yA$��A#�
A#C�A"��A"VA!��A �HAp�A%A�AE�AA�A�A�-A�7A�A��A{AdZA\)AjA|�A�jAI�A33A��AbA\)AM�A�PAZAJA
��A
ffA	��A	XA�jAv�AJA��AO�A
=A�AK�A��A�9A �Al�A7LA �j@��P@�n�@���@��h@���@���@�l�@�~�@�Ĝ@�n�@�x�@띲@�/@�x�@㕁@�33@�/@��@߮@�"�@���@ݺ^@�Z@۶F@�;d@ڇ+@��@��@ְ!@�9X@�ƨ@ӥ�@ӕ�@��@ӥ�@�+@҇+@�1'@Ο�@�-@���@��@���@�ȴ@��H@��H@�ȴ@��@��@ʟ�@�v�@ǅ@�v�@�$�@ŉ7@Õ�@�n�@��`@�I�@��@�S�@���@�^5@��^@��@�1'@���@�S�@��H@���@�x�@�9X@�b@�bN@��u@��@�+@��P@���@��-@�x�@��j@���@�33@�ff@���@���@���@��@��@�I�@��@�+@��!@�5?@���@�O�@�p�@���@�@�n�@��\@�v�@�V@�7L@��@�Ĝ@�\)@��y@�M�@�`B@�j@��u@���@�j@�9X@��@�(�@���@�K�@�
=@��+@�M�@�=q@��@��u@��@��y@�n�@��@��y@��y@�~�@�E�@���@���@�Ĝ@��9@���@���@��@���@��@�r�@��@��F@�t�@��@�-@��@��^@�hs@�&�@��@�Ĝ@���@�r�@�I�@�9X@�b@��F@�|�@�33@���@��R@�ff@���@�p�@�p�@�x�@�hs@�Ĝ@���@��F@���@�33@��y@��+@�^5@�^5@�~�@�ȴ@��@��+@��^@���@�5?@���@��9@���@��j@��u@��@�"�@���@���@��@��@�ff@��#@���@���@��h@�X@�&�@�G�@�hs@�X@��j@�Q�@��D@��9@��@�Z@�9X@��@��
@�\)@���@��@��@�{@�$�@�$�@�$�@�$�@�$�@�$�@���@��T@�@�x�@�`B@��@��@��@��@�z�@��1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 A�9XA�;dA�A�A�C�A�K�A�K�A�M�A�I�A�M�A�M�A�E�A�C�A�9XA�=qA��A��9A�n�A�t�A��^A�&�A���A�ƨA��jA��PA�M�A�?}A�"�A�{A��A�ȴA�\)A���A��;A�M�A�`BA�+A��A�C�A��+A��-A�?}A��jA��\A�z�A�^5A�?}A��A��A��!A���A���A��A�^5A�K�A�1'A���A�+A��9A��A��A�(�A��hA�O�A��mA��A�x�A�I�A�=qA�bA�\)A��+A��uA�`BA��A��!A��A��^A�I�A���A��TA��#A��uA��+A�ffA�-A�t�A���A�z�A���A���A��A���A��A��A��HA�ZA�
=A��A��7A���A���A�?}A��A��A���A��A�r�A��FA�  A�r�A��A�  A� �A� �A�ȴA�^5A��A|�A~ȴA~�jA~jA}��A|�\A{�;A{�hAz�AyK�AxI�Aw��AvVAu\)At��As|�Ar�Aq
=Ao?}An�AnbNAm�Ak&�AjZAiG�AgAdn�Ab�\Aa"�A_�wA_?}A]��A\(�A[�wAZA�AX��AWXAVbNAQ�;AN��AM�AL(�AK|�AJ��AJJAIG�AH�yAHQ�AF�AFE�AE�AC7LAAAA;dA?�A=�A<A�A9XA8�+A7K�A5��A5?}A5`BA5\)A5A4I�A2�A0  A-hsA+��A*��A)�hA)/A(�!A({A'XA&z�A&�A%/A$�yA$��A#�
A#C�A"��A"VA!��A �HAp�A%A�AE�AA�A�A�-A�7A�A��A{AdZA\)AjA|�A�jAI�A33A��AbA\)AM�A�PAZAJA
��A
ffA	��A	XA�jAv�AJA��AO�A
=A�AK�A��A�9A �Al�A7LA �j@��P@�n�@���@��h@���@���@�l�@�~�@�Ĝ@�n�@�x�@띲@�/@�x�@㕁@�33@�/@��@߮@�"�@���@ݺ^@�Z@۶F@�;d@ڇ+@��@��@ְ!@�9X@�ƨ@ӥ�@ӕ�@��@ӥ�@�+@҇+@�1'@Ο�@�-@���@��@���@�ȴ@��H@��H@�ȴ@��@��@ʟ�@�v�@ǅ@�v�@�$�@ŉ7@Õ�@�n�@��`@�I�@��@�S�@���@�^5@��^@��@�1'@���@�S�@��H@���@�x�@�9X@�b@�bN@��u@��@�+@��P@���@��-@�x�@��j@���@�33@�ff@���@���@���@��@��@�I�@��@�+@��!@�5?@���@�O�@�p�@���@�@�n�@��\@�v�@�V@�7L@��@�Ĝ@�\)@��y@�M�@�`B@�j@��u@���@�j@�9X@��@�(�@���@�K�@�
=@��+@�M�@�=q@��@��u@��@��y@�n�@��@��y@��y@�~�@�E�@���@���@�Ĝ@��9@���@���@��@���@��@�r�@��@��F@�t�@��@�-@��@��^@�hs@�&�@��@�Ĝ@���@�r�@�I�@�9X@�b@��F@�|�@�33@���@��R@�ff@���@�p�@�p�@�x�@�hs@�Ĝ@���@��F@���@�33@��y@��+@�^5@�^5@�~�@�ȴ@��@��+@��^@���@�5?@���@��9@���@��j@��u@��@�"�@���@���@��@��@�ff@��#@���@���@��h@�X@�&�@�G�@�hs@�X@��j@�Q�@��D@��9@��@�Z@�9X@��@��
@�\)@���@��@��@�{@�$�@�$�@�$�@�$�@�$�@�$�@���@��T@�@�x�@�`B@��@��@��@��@�z�@��1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 ;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oB�!B�!B�!B�!B�B�B�B�!B�B�B�!B�!B�'B�'B�3B�^B��B��B��B��B��B��B��B��B��B��B��B��B��B�)B��BɺB�B��B1BB\B�B33BP�B^5BhsBm�Bp�Bs�Bv�By�B~�B�%B�DB�JB�PB�\B�hB�oB�oB�B\)BM�BN�BM�BD�B@�B5?B%�B�B,B1'B2-BE�BQ�BXBVBT�BO�BB�B5?BB�;B��B��Bx�BffBF�B'�BB�HB��B��B�jB�B��B��B�JB�By�Bs�BgmBR�BK�B?}B)�B�B�B�B�BuBoB
��B
�yB
��B
�wB
�-B
��B
�1B
�B
v�B
hsB
cTB
dZB
cTB
e`B
ffB
dZB
aHB
]/B
XB
R�B
N�B
H�B
D�B
?}B
8RB
0!B
)�B
"�B
�B
�B
uB

=B
+B
B	��B	�B	�/B	�B	��B	��B	ƨB	�wB	�dB	�B	��B	�uB	�+B	[#B	8RB	)�B	&�B	$�B	#�B	'�B	-B	,B	'�B	�B	uB	JB	B��B��B�B�sB�B��B��B��B��B�
B�;B�TB�mB�fB�
B�^B��B��B��B��B�uB�oB�hB�bB�VB�JB�PB�bB�uB�{B�uB�uB�hB�hB�bB�bB�\B�\B�VB�PB�VB�\B�\B�\B�\B�PB�JB�7B�%B�B�B�B� B}�Bz�Bx�Bv�Bs�Bp�Bk�BhsBgmBffBdZBcTBdZBcTBbNBaHB`BB^5B]/BZBYBXBVBT�BT�BR�BR�BQ�BP�BN�BK�BJ�BJ�BH�BF�BK�BL�BN�BD�B?}B?}B?}B=qB>wB>wB=qB=qBD�BJ�BK�BJ�BG�BE�BC�BB�B@�B@�BB�BC�BE�BG�BI�BH�BF�BJ�BJ�BJ�BG�BH�BS�B]/B`BB`BBcTBdZBffBdZBhsBjBk�Bl�Bk�Bs�Bu�Bv�Bv�Bv�Bv�Bv�Bu�Bu�Bu�Bu�Bu�Bu�Bt�Bu�Bx�B}�B�B�B�=B�%B�B� B~�B~�B� B� B�B�B�%B�%B�7B�PB�\B�bB�{B��B��B��B��B�B�RB�jBŢB��B��B��B�
B�B�B�#B�BB�BB�BB�TB�`B�B�B�B�B	  B	B	JB	PB	\B	oB	uB	uB	�B	�B	�B	�B	�B	�B	�B	"�B	+B	1'B	2-B	2-B	49B	6FB	;dB	>wB	A�B	D�B	E�B	H�B	K�B	M�B	N�B	Q�B	W
B	YB	ZB	\)B	^5B	`BB	aHB	bNB	cTB	dZB	e`B	ffB	iyB	jB	l�B	m�B	o�B	p�B	s�B	t�B	u�B	u�B	v�B	v�B	x�B	y�B	{�B	{�B	{�B	|�B	}�B	}�B	~�B	�B	�B	�B	�B	�B	�7B	�=B	�JB	�bB	�hB	�oB	�hB	�hB	�uB	�{B	�{B	�{B	�uB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�B	�!B	�'B	�'B	�'B	�9B	�LB	�RB	�XB	�XB	�XB	�XB	�XB	�XB	�dB	�dB	�jB	�qB	�wB	�}B	�}B	��B	��B	��B	�
1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 B�!B�!B�!B�!B�B�B�B�!B�B�B�!B�!B�'B�-B�?B�}B��B�B�B��B��B��B��B��B��B��B��B��B��B�5B��BȴB�B��B	7BBVB�B2-BR�BaHBiyBm�Bq�Bt�Bw�Bz�B� B�%B�DB�PB�VB�\B�oB�uB��B�=BaHBN�BP�BP�BF�BB�B:^B)�B�B,B2-B0!BD�BQ�BYBXBXBR�BD�B;dB	7B�NB�B�B|�Bn�BK�B/BbB�fB�B��BĜB�B��B��B�uB�B{�By�Bo�BW
BR�BJ�B.B!�B�B�B�B�B�B%B
��B
��B
B
�jB
��B
�=B
�B
{�B
jB
cTB
e`B
e`B
hsB
hsB
e`B
dZB
aHB
[#B
T�B
R�B
K�B
F�B
C�B
<jB
33B
/B
#�B
!�B
�B
�B
PB

=B
+B
  B	�B	�HB	�/B	�B	��B	ɺB	�}B	�wB	�'B	��B	��B	�oB	bNB	<jB	-B	(�B	&�B	%�B	)�B	.B	.B	,B	�B	�B	oB	%B	  B��B��B�B�HB��B��B�
B��B�
B�;B�ZB�yB�B�5BB�B��B��B��B��B�{B�{B�uB�bB�\B�VB�hB��B��B�{B��B�{B�{B��B�oB�bB�hB�bB�bB�hB�hB�bB�hB�hB�\B�\B�\B�7B�%B�B�B�B~�B|�Bz�By�Bu�Bs�Bq�Bl�BhsBgmBffBe`Be`BdZBcTBbNBaHBaHB`BB_;B]/BZBXBVBW
BVBT�BVBT�BR�BN�BL�BL�BK�BJ�BL�BN�BP�BH�BA�B@�BA�B>wB?}B?}B>wB?}BF�BK�BL�BK�BH�BF�BG�BE�BA�B@�BB�BC�BF�BH�BJ�BL�BI�BK�BK�BL�BK�BH�BS�B]/B`BB`BBcTBe`BgmBiyBjBk�Bl�Bo�Bm�Bv�Bv�Bw�Bw�Bw�Bv�Bw�Bv�Bw�Bv�Bv�Bv�Bv�Bv�Bw�Bx�B}�B�B�B�\B�JB�B�B� B�B�B�B�B�B�+B�1B�=B�PB�bB�hB��B��B��B��B��B�B�RB�jBŢB��B��B��B�B�B�B�/B�HB�HB�HB�ZB�`B�B�B�B�B	B	B	PB	PB	bB	oB	uB	{B	�B	�B	�B	�B	�B	�B	�B	#�B	+B	2-B	33B	2-B	49B	6FB	;dB	>wB	A�B	D�B	E�B	I�B	K�B	M�B	O�B	R�B	W
B	YB	[#B	\)B	^5B	`BB	aHB	bNB	cTB	dZB	e`B	gmB	iyB	jB	l�B	m�B	p�B	q�B	s�B	t�B	u�B	u�B	w�B	w�B	x�B	y�B	|�B	|�B	|�B	|�B	}�B	}�B	~�B	�B	�B	�%B	�B	�B	�DB	�=B	�JB	�bB	�hB	�{B	�oB	�oB	�uB	�{B	�{B	��B	�{B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�B	�'B	�-B	�-B	�-B	�9B	�LB	�RB	�XB	�XB	�XB	�XB	�XB	�XB	�dB	�dB	�qB	�qB	�}B	�}B	��B	��B	��B	��B	�
1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 <#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
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
<49X<#�
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
PRES            TEMP            PSAL            PRES_ADJUSTED = PRES - dP                                                                                                                                                                                                                                       none                                                                                                                                                                                                                                                            PSAL_ADJUSTED = sw_salt( sw_cndr(PSAL,TEMP,PRES), TEMP, PRES_ADJUSTED ) + CTM                                                                                                                                                                                   dP =-0.1 dbar.                                                                                                                                                                                                                                                  none                                                                                                                                                                                                                                                            CTM: alpha=0.141, tau=6.68s                                                                                                                                                                                                                                     Pressures adjusted by using pressure offset at the sea surface. The quoted error is manufacturer specified accuracy in dbar.                                                                                                                                    The quoted error is manufacturer specified accuracy with respect to ITS-90 at time of laboratory calibration.                                                                                                                                                   No significant salinity drift detected. Salinity adjusted for effects of pressure adjustment. Conductivity cell thermal mass correction (CTM) applied to measurements taken in Cp mode. The quoted error is max[0.01, sqrt(OWerr^2+CTMerr^2)] in PSS-78.        201201031447212012010314472120120103144721  AO  ARGQ                                                                        20111130142221  QCP$                G�O�G�O�G�O�FFBFE           AO  ARGQ                                                                        20111130142221  QCF$                G�O�G�O�G�O�0               UW  ARSQUWQC    WOD & nearby Argo as visual check                               20120103144721  IP                  G�O�G�O�G�O�                