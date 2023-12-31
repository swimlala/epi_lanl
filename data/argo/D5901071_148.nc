CDF      
      	DATE_TIME         STRING2       STRING4       STRING8       STRING16      STRING32       STRING64   @   	STRING256         N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       D2011-11-30 AOML 2.2 creation; 2015-12-13T22:40:32Z UW 3.1 conversion   
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
_FillValue                 �  A8   PRES_ADJUSTED            
      	   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     units         decibar    	valid_min                	valid_max         F;�    
resolution        =���   C_format      %7.1f      FORTRAN_format        F7.1f      
_FillValue        G�O�     �  C    PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  J�   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     units         decibar    
resolution        =���   C_format      %7.1f      FORTRAN_format        F7.1f      
_FillValue        G�O�     �  L�   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     	valid_min         �      	valid_max         B      
resolution        :�o   C_format      %9.3f      FORTRAN_format        F9.3f      
_FillValue        G�O�     �  T8   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  [�   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     	valid_min         �      	valid_max         B      
resolution        :�o   C_format      %9.3f      FORTRAN_format        F9.3f      
_FillValue        G�O�     �  ]�   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  eP   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     units         degree_Celsius     
resolution        :�o   C_format      %9.3f      FORTRAN_format        F9.3f      
_FillValue        G�O�     �  g8   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    	valid_min         @      	valid_max         B$     
resolution        :�o   C_format      %9.3f      FORTRAN_format        F9.3f      
_FillValue        G�O�     �  n�   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  vh   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    	valid_min         @      	valid_max         B$     
resolution        :�o   C_format      %9.3f      FORTRAN_format        F9.3f      
_FillValue        G�O�     �  xP   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     units         psu    
resolution        :�o   C_format      %9.3f      FORTRAN_format        F9.3f      
_FillValue        G�O�     �  ��   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  0  �h   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                    ��   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                    ��   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                    ��   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  ,  ��   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                    ��   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                    ��   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                    ��   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                    ��   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  @  ��   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                    �   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                    �$   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    �(   HISTORY_START_PRES                    	long_name          Start pressure action applied on   units         decibar    
_FillValue        G�O�        �8   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    units         decibar    
_FillValue        G�O�        �<   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�        �@   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    �DArgo profile    3.1 1.2 19500101000000  5901071 US ARGO PROJECT                                                 Stephen Riser                                                   PRES            TEMP            PSAL               �A   AO  20111130142527  20190522121827  1727_5046_148                   2C  D   APEX                            2143                            040306                          846 @����. 1   @���s��@60�`A�7�c�V�1   GPS     Primary sampling: mixed [deeper than nominal 980dbar: discrete; nominal 980dbar to surface: 2dbar-bin averaged]                                                                                                                                                    A   A   A   @�  @�  A   A   A@  A`  A�  A�  A�  A�  A�  A�  A�  A�  B   B  B  B  B   B'��B/��B7��B@  BH  BP  BX  B`  Bh  Bp  Bx  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B���B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�33B�  B�  B���C  C  C  C  C
  C  C  C  C  C  C  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB�CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C��C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� DfD� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D�fD  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4fD4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dj��Dky�Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� 111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  @�33@�33A��A!��AA��Aa��A���A���A���A���A���A���A���A���B ffBffBffBffB ffB(  B0  B8  B@ffBHffBPffBXffB`ffBhffBpffBxffB�33B�33B�33B�33B�33B�33B�33B�33B�33B�33B�33B�33B�33B�33B�33B�  B�33B�33B�33B�33B�33B�33B�33B�33B�33B�33B�33B�33B�33B�ffB�33B�33C   C�C�C�C�C
�C�C�C�C�C�C�C�C�C�C�C �C"�C$�C&�C(�C*�C,�C.�C0�C2�C4�C6�C8�C:�C<�C>�C@�CB33CD�CF�CH�CJ�CL�CN�CP�CR�CT�CV�CX�CZ�C\�C^�C`�Cb�Cd�Cf�Ch�Cj�Cl�Cn�Cp�Cr�Ct�Cv�Cx�Cz�C|�C~�C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C�  C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C�  C��C��C��C��C��C��C��C��C��C��D fD �fDfD�fDfD�fDfD�fD�D�fDfD�fDfD�fDfD�fDfD�fD	fD	�fD
fD
�fDfD�fDfD�fDfD�fDfD�fDfD�fDfD�fDfD�fDfD�fDfD�fDfD�fDfD�fDfD�fDfD�fDfD�fDfD�fDfD�fDfD�fDfD��DfD�fDfD�fDfD�fD fD �fD!fD!�fD"fD"�fD#fD#�fD$fD$�fD%fD%�fD&fD&�fD'fD'�fD(fD(�fD)fD)�fD*fD*�fD+fD+�fD,fD,�fD-fD-�fD.fD.�fD/fD/�fD0fD0�fD1fD1�fD2fD2�fD3fD3�fD4�D4�fD5fD5�fD6fD6�fD7fD7�fD8fD8�fD9fD9�fD:fD:�fD;fD;�fD<fD<�fD=fD=�fD>fD>�fD?fD?�fD@fD@�fDAfDA�fDBfDB�fDCfDC�fDDfDD�fDEfDE�fDFfDF�fDGfDG�fDHfDH�fDIfDI�fDJfDJ�fDKfDK�fDLfDL�fDMfDM�fDNfDN�fDOfDO�fDPfDP�fDQfDQ�fDRfDR�fDSfDS�fDTfDT�fDUfDU�fDVfDV�fDWfDW�fDXfDX�fDYfDY�fDZfDZ�fD[fD[�fD\fD\�fD]fD]�fD^fD^�fD_fD_�fD`fD`�fDafDa�fDbfDb�fDcfDc�fDdfDd�fDefDe�fDffDf�fDgfDg�fDhfDh�fDifDi�fDjfDj�fDk  Dk� DlfDl�fDmfDm�fDnfDn�fDofDo�fDpfDp�fDqfDq�fDrfDr�fDsfDs�f111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  @��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��A�v�A�hsA�XA�VA�ZA�XA�XA�VA�M�A�I�A��HA�`BAʴ9A�33Aȡ�A�M�AƾwA�9XAżjA��`AhA���A��+A��-A�~�A���A�^5A�1A�G�A��A��PA� �A��jA�ffA��HA��uA�VA��A��uA�-A�dZA��yA���A�G�A��wA���A��yA�\)A�A�l�A�%A��/A�bNA���A���A�r�A�\)A�1A�5?A���A�^5A�oA���A���A��uA���A�VA��7A��TA�t�A� �A�VA���A�33A���A��A��DA��mA���A��^A�E�A��;A�-A�
=A��#A�;dA���A�5?A�XA� �A�\)A���A�A�A�x�A��A��;A���A��jA��7A�VA�%A�A�A��RA�z�A���A�~�A��TA�`BA~�\Az~�AyS�Ax�jAw��AvȴAu�PAt(�Aq�mAp��Apn�ApM�Ao�Ao+Al��Aj��Aj~�Ah�Ag��Af�/Ad1Ab��Aa&�A_"�A^AZ^5AXI�AV�9AU��AS��AP5?AO�ANI�AM�FAKt�AJ�AH�AF��AEXAD��AD��ADv�ADI�AC��AA�A@��A?�^A>�A=��A=G�A<M�A<A;�
A:jA9XA8ZA7�wA6bNA5��A4�!A2~�A0�yA.�A-�TA,ZA+��A+x�A*�A)A'33A&M�A$�RA"�A!C�A ��A �jA �9A ��A �DA"�A�A~�A  A�TA�AXA&�AjA?}A9XA��A��A-A��AO�AdZAĜA�9Az�A�;AG�A
=A
�/A
��A
�A��AȴA�A+A1'A"�@��\@�G�@�&�@���@�  @�dZ@�Q�@��!@���@�@�F@��@�Q�@�33@�{@�O�@�Ĝ@�A�@�dZ@�"�@��@�+@�J@�bN@�=q@��@��@ߥ�@އ+@�z�@�33@�~�@��@�+@֗�@ղ-@���@мj@϶F@Ώ\@�G�@ˮ@��@�/@�z�@�j@� �@�  @��m@�@�7L@��/@�j@��m@�K�@§�@�ff@��T@��@�\)@�"�@�V@���@��^@��7@�?}@�7L@�/@���@���@��@���@�Q�@��H@���@�V@��T@��h@��7@��@�/@�Ĝ@�Z@�33@���@��+@�n�@�V@�-@��@�M�@�hs@��u@�Q�@�b@�  @���@��;@�\)@�=q@��@��@���@��#@�V@�b@��T@���@�x�@��/@�Z@�1'@��;@���@�C�@�~�@��9@�A�@�(�@�  @��m@��;@���@��w@��@��P@��P@�|�@�t�@�l�@�l�@�\)@�C�@��@�o@�
=@���@��\@��@���@�hs@�O�@�G�@�G�@�7L@��@���@�bN@�(�@��F@�t�@�K�@���@�$�@�J@�@�Ĝ@�b@��F@�
=@���@��\@��+@�n�@�V@�M�@�=q@�=q@�=q@�5?@�{@���@�O�@�%@��@��@�@�x�@�x�@���@��-@���@�G�@�%@�%@�Q�@�33@�~�@��-@�x�@�hs@�p�@�p�@�p�@�`B@�V@��@���@��9@�j@�A�@�  @��F@�|�@�S�@��y@���@�5?@��@�@��#@��^@���@��@�X@�O�@�X@�?}@��@���@��/@��@��D@�  @�ƨ@�l�@�o@�@���@�ff@�=q@��@�@���@��h@��@�/@��@���@���@���@�%@��`@�(�@��;@�|�@��@��@�ȴ@���@��\@�ff@�-@��T@��`@�1'@���@�;d@��@��@��@���@���@��\@�v�@�^5@�-111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  A�v�A�hsA�XA�VA�ZA�XA�XA�VA�M�A�I�A��HA�`BAʴ9A�33Aȡ�A�M�AƾwA�9XAżjA��`AhA���A��+A��-A�~�A���A�^5A�1A�G�A��A��PA� �A��jA�ffA��HA��uA�VA��A��uA�-A�dZA��yA���A�G�A��wA���A��yA�\)A�A�l�A�%A��/A�bNA���A���A�r�A�\)A�1A�5?A���A�^5A�oA���A���A��uA���A�VA��7A��TA�t�A� �A�VA���A�33A���A��A��DA��mA���A��^A�E�A��;A�-A�
=A��#A�;dA���A�5?A�XA� �A�\)A���A�A�A�x�A��A��;A���A��jA��7A�VA�%A�A�A��RA�z�A���A�~�A��TA�`BA~�\Az~�AyS�Ax�jAw��AvȴAu�PAt(�Aq�mAp��Apn�ApM�Ao�Ao+Al��Aj��Aj~�Ah�Ag��Af�/Ad1Ab��Aa&�A_"�A^AZ^5AXI�AV�9AU��AS��AP5?AO�ANI�AM�FAKt�AJ�AH�AF��AEXAD��AD��ADv�ADI�AC��AA�A@��A?�^A>�A=��A=G�A<M�A<A;�
A:jA9XA8ZA7�wA6bNA5��A4�!A2~�A0�yA.�A-�TA,ZA+��A+x�A*�A)A'33A&M�A$�RA"�A!C�A ��A �jA �9A ��A �DA"�A�A~�A  A�TA�AXA&�AjA?}A9XA��A��A-A��AO�AdZAĜA�9Az�A�;AG�A
=A
�/A
��A
�A��AȴA�A+A1'A"�@��\@�G�@�&�@���@�  @�dZ@�Q�@��!@���@�@�F@��@�Q�@�33@�{@�O�@�Ĝ@�A�@�dZ@�"�@��@�+@�J@�bN@�=q@��@��@ߥ�@އ+@�z�@�33@�~�@��@�+@֗�@ղ-@���@мj@϶F@Ώ\@�G�@ˮ@��@�/@�z�@�j@� �@�  @��m@�@�7L@��/@�j@��m@�K�@§�@�ff@��T@��@�\)@�"�@�V@���@��^@��7@�?}@�7L@�/@���@���@��@���@�Q�@��H@���@�V@��T@��h@��7@��@�/@�Ĝ@�Z@�33@���@��+@�n�@�V@�-@��@�M�@�hs@��u@�Q�@�b@�  @���@��;@�\)@�=q@��@��@���@��#@�V@�b@��T@���@�x�@��/@�Z@�1'@��;@���@�C�@�~�@��9@�A�@�(�@�  @��m@��;@���@��w@��@��P@��P@�|�@�t�@�l�@�l�@�\)@�C�@��@�o@�
=@���@��\@��@���@�hs@�O�@�G�@�G�@�7L@��@���@�bN@�(�@��F@�t�@�K�@���@�$�@�J@�@�Ĝ@�b@��F@�
=@���@��\@��+@�n�@�V@�M�@�=q@�=q@�=q@�5?@�{@���@�O�@�%@��@��@�@�x�@�x�@���@��-@���@�G�@�%@�%@�Q�@�33@�~�@��-@�x�@�hs@�p�@�p�@�p�@�`B@�V@��@���@��9@�j@�A�@�  @��F@�|�@�S�@��y@���@�5?@��@�@��#@��^@���@��@�X@�O�@�X@�?}@��@���@��/@��@��D@�  @�ƨ@�l�@�o@�@���@�ff@�=q@��@�@���@��h@��@�/@��@���@���@���@�%@��`@�(�@��;@�|�@��@��@�ȴ@���@��\@�ff@�-@��T@��`@�1'@���@�;d@��@��@��@���@���@��\@�v�@�^5@�-111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  ;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oB�B�B�B�B�B�B�B�B�B�B%BE�BZBaHBy�B�JB�JB�VB�PB�JB�oB�{B��B�B�B��B��B��B�#B�/B�5B�#B�B�B�B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��BĜB�}B��B�}B��B�qB�FB�'B�B��B�1BbNBP�BK�BC�B;dB49B$�B�B
=B��B��B�B�NB��B��B��BǮB�^B�B�B��B��B�B^5B-BhB
��B
�B
�B
��B
�RB
�LB
�LB
�LB
�RB
�^B
�FB
�B
��B
�%B
}�B
y�B
o�B
iyB
T�B
A�B
>wB
?}B
8RB
5?B
.B
"�B
�B
�B
uB
oB
\B
	7B	��B	�B	�B	�mB	�NB	�#B	��B	ĜB	�qB	�3B	�B	��B	�\B	�+B	�B	s�B	jB	`BB	\)B	ZB	Q�B	I�B	C�B	<jB	8RB	7LB	7LB	7LB	6FB	33B	)�B	$�B	�B	�B	�B	�B	uB	uB	hB	
=B	B��B��B��B�B�B�HB�B��BɺBĜB��B�wB�XB�'B�B��B��B��B��B��B��B��B��B��B�uB�bB�VB�PB�JB�DB�=B�1B�B�B}�Bz�Bw�Bt�Bq�Bo�Bn�Bn�Bn�Bm�Bk�Bk�Bk�BjBhsBffBbNBaHB]/B[#BXBW
BT�BT�BT�BR�BQ�BO�BM�BM�BK�BK�BK�BI�BJ�BJ�BJ�BK�BK�BJ�BK�BK�BJ�BJ�BI�BH�BI�BJ�BJ�BI�BH�BI�BJ�BI�BJ�BK�BJ�BI�BM�BN�BN�BO�BQ�BVBYBZB]/B]/B^5B^5B]/BbNBdZBdZBe`BffBhsBjBjBjBm�Bq�Br�Bt�Bu�Bv�Bv�Bw�Bw�Bv�Bv�Bx�B{�By�B�B�+B�1B�7B�=B�JB�JB�JB�JB�PB�VB�hB�oB�oB�oB�oB�uB�uB�uB��B��B��B��B��B��B��B��B��B��B��B�B�'B�9B�LBÖBŢBƨB��B��B��B��B�
B�#B�NB�B��B��B��B��B��B��B��B��B	  B	B	B	B	B	B	B	B	B	B	B	B	1B	PB	hB	�B	�B	�B	�B	�B	$�B	)�B	,B	-B	2-B	6FB	=qB	@�B	F�B	I�B	N�B	R�B	VB	XB	[#B	_;B	aHB	dZB	e`B	ffB	ffB	gmB	gmB	gmB	gmB	hsB	jB	l�B	o�B	w�B	|�B	� B	�B	�B	�B	�B	�B	�B	�B	�1B	�1B	�B	�B	�B	�B	�B	�B	�B	�B	�+B	�7B	�DB	�DB	�JB	�JB	�PB	�VB	�bB	�oB	�uB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�'B	�3B	�3B	�?B	�?B	�?B	�?B	�?B	�FB	�FB	�FB	�FB	�LB	�XB	�qB	�wB	�}B	B	B	B	ÖB	ÖB	ĜB	ĜB	ŢB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  B�B�B�B�B�B�B�B�B�B�BJBH�B\)BffB~�B�VB�VB�bB�{B�hB��B��B��B�B�-BB��B��B�/B�;B�BB�/B�B�#B�B�
B�B�B��B��B�B��B��B�B��B��B��B��B��B��B��B��B��BȴB��B��BŢBŢB�}B�LB�-B�B��B�oBiyBR�BN�BF�B=qB:^B)�B�BhB��B��B��B�B��B��B��B��B�wB�B�B�B��B�bBs�B:^B�B  B
�B
��B
�B
�RB
�LB
�LB
�RB
�XB
�dB
�XB
�3B
��B
�7B
~�B
|�B
q�B
o�B
_;B
D�B
@�B
B�B
;dB
9XB
2-B
)�B
!�B
�B
uB
uB
oB
bB
B	��B	��B	�B	�ZB	�NB	��B	ȴB	B	�FB	�?B	��B	�oB	�7B	�B	y�B	l�B	bNB	]/B	_;B	T�B	N�B	F�B	?}B	9XB	8RB	7LB	8RB	8RB	8RB	-B	&�B	"�B	�B	�B	�B	{B	{B	�B	PB	+B	B	  B��B��B�B�fB�;B��B��BƨBB��B��B�LB�B�B��B��B��B��B��B��B��B��B��B��B�bB�VB�PB�JB�DB�DB�VB�%B�B}�B|�By�Bu�Bt�Bp�Bn�Bo�Bo�Bm�Bl�Bl�Bk�BjBjBgmBgmBaHB^5B[#B\)BW
BT�BVBS�BR�BT�BP�BP�BP�BN�BM�BM�BL�BL�BK�BL�BL�BL�BL�BL�BK�BK�BL�BL�BK�BL�BK�BK�BL�BK�BL�BL�BM�BL�BL�BN�BQ�BP�BP�BR�BT�BYB[#BZB]/B]/B^5B^5B_;BcTBe`Be`BffBgmBiyBjBk�Bl�Bo�Bq�Bs�Bu�Bu�Bv�Bw�Bw�Bw�Bw�Bx�By�B|�B}�B�B�1B�7B�=B�DB�JB�JB�PB�PB�VB�bB�oB�oB�oB�oB�oB�uB�uB��B��B��B��B��B��B��B��B��B��B��B��B�B�-B�FB�dBÖBŢBǮB��B��B��B�B�B�)B�ZB�B��B��B��B��B��B��B��B��B	  B	B	B	B	B	B	B	B	B	B	B	B		7B	VB	hB	�B	�B	�B	�B	 �B	%�B	)�B	,B	.B	2-B	6FB	>wB	A�B	F�B	J�B	P�B	S�B	W
B	YB	\)B	_;B	aHB	dZB	e`B	ffB	ffB	gmB	gmB	gmB	gmB	hsB	k�B	l�B	n�B	v�B	|�B	� B	�B	�B	�B	�B	�%B	�B	�B	�7B	�=B	�B	�B	�B	�B	�B	�B	�B	�B	�1B	�7B	�DB	�DB	�PB	�JB	�PB	�\B	�bB	�oB	�{B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�'B	�3B	�9B	�?B	�?B	�?B	�FB	�?B	�FB	�FB	�FB	�FB	�LB	�^B	�wB	�}B	��B	B	B	B	ÖB	ÖB	ĜB	ŢB	ǮB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  <#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
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
<T��<�1<T��<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
PRES            TEMP            PSAL            PRES_ADJUSTED = PRES - dP                                                                                                                                                                                                                                       none                                                                                                                                                                                                                                                            PSAL_ADJUSTED = sw_salt( sw_cndr(PSAL,TEMP,PRES), TEMP, PRES_ADJUSTED ) + CTM                                                                                                                                                                                   dP =-0.1 dbar.                                                                                                                                                                                                                                                  none                                                                                                                                                                                                                                                            CTM: alpha=0.141, tau=6.68s                                                                                                                                                                                                                                     Pressures adjusted by using pressure offset at the sea surface. The quoted error is manufacturer specified accuracy in dbar.                                                                                                                                    The quoted error is manufacturer specified accuracy with respect to ITS-90 at time of laboratory calibration.                                                                                                                                                   No significant salinity drift detected. Salinity adjusted for effects of pressure adjustment. Conductivity cell thermal mass correction (CTM) applied to measurements taken in Cp mode. The quoted error is max[0.01, sqrt(OWerr^2+CTMerr^2)] in PSS-78.        201201031447262012010314472620120103144726  AO  ARGQ                                                                        20111130142527  QCP$                G�O�G�O�G�O�FFBFE           AO  ARGQ                                                                        20111130142527  QCF$                G�O�G�O�G�O�0               UW  ARSQUWQC    WOD & nearby Argo as visual check                               20120103144726  IP                  G�O�G�O�G�O�                