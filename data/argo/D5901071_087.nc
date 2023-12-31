CDF      
      	DATE_TIME         STRING2       STRING4       STRING8       STRING16      STRING32       STRING64   @   	STRING256         N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       D2011-11-30 AOML 2.2 creation; 2015-12-13T22:40:15Z UW 3.1 conversion   
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
_FillValue                    ��Argo profile    3.1 1.2 19500101000000  5901071 US ARGO PROJECT                                                 Stephen Riser                                                   PRES            TEMP            PSAL               WA   AO  20111130141131  20190522121826  1727_5046_087                   2C  D   APEX                            2143                            040306                          846 @Ԇ�� 1   @Ԇ�`
@7�+I��ddZ�1   GPS     Primary sampling: mixed [deeper than nominal 980dbar: discrete; nominal 980dbar to surface: 2dbar-bin averaged]                                                                                                                                                    A   A   B   @9��@�  @�  A   A   A@  A`  A�  A�  A�  A�  A���A�  A�  A�  A�33B  B  B  B   B(  B/��B7��B@  BH  BP  BX  B`ffBh  Bo��Bw��B��B�  B�  B�  B�  B�  B�33B�  B���B�  B�  B���B�33B�33B�  B�  B�33B�  B�  B���B�  B�33B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  C   C  C  C�fC�fC
  C�C�C  C  C  C  C  C  C�fC  C �C"�C$�C&  C'�fC)�fC+�fC-�fC/�fC2  C4  C6  C8�C:�C<  C=�fC@  CB  CD�CF  CH  CJ  CL  CN  CP  CR  CS�fCV  CX  CZ  C\  C^�C`�Cb�Cd�Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct�Cv�Cx  Cz  C|  C}�fC�  C�  C�  C��3C�  C��C��C�  C�  C�  C�  C�  C��3C�  C�  C��C��C�  C��3C�  C�  C�  C��C�  C�  C�  C�  C��C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C�  C�  C��3C��3C��3C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C�  C�  C�  C�  C�  C��C��C�  C�  C��C��3C�  C�  C�  C�  C��3C�  C�  C�  C�  C��C��C�  C�  C��C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C��3C�  C��C�  C�  C��C�  C�  D   D y�D  D� D��D� D  D� D  D� D  D� DfD� D��D� D  D� D	  D	� D
  D
y�D
��D� DfD�fDfD�fDfD� D��Dy�D��D� D  D� D��D� D��Dy�D  D�fD  Dy�D  D� D  D� DfD�fD  D� D  D� DfD� D  Dy�D  D�fD  D� D��D� D fD � D!  D!� D!��D"� D#fD#� D$  D$y�D$��D%� D&fD&� D&��D'� D(  D(y�D)  D)�fD*fD*y�D*��D+�fD,fD,�fD-fD-�fD.  D.� D/  D/y�D0  D0� D1  D1� D1��D2� D3  D3� D4fD4� D5  D5� D6  D6� D7fD7�fD8  D8� D9  D9� D:  D:� D;  D;�fD<fD<�fD=fD=� D=��D>y�D>��D?� D@  D@� DA  DA�fDB  DBy�DC  DC� DD  DD� DE  DE� DF  DF�fDG  DGy�DH  DH� DI  DIy�DJ  DJ�fDK  DK� DL  DL� DM  DM�fDN  DN� DO  DO� DPfDP�fDQ  DQy�DQ��DR� DS  DS� DT  DT� DU  DU� DVfDV� DW  DWy�DX  DX�fDYfDY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^�fD_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd�fDefDe�fDf  Df� DgfDg�fDhfDh� Dh��Di� DjfDj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dny�Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dy��D�,�D�ffD���D���D�6fD�l�D��fD���D��D�ffD���D��3D�)�D�L�Dڠ D�� D��D�FfD�fD�	�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111@9��@�  @�  A   A   A@  A`  A�  A�  A�  A�  A���A�  A�  A�  A�33B  B  B  B   B(  B/��B7��B@  BH  BP  BX  B`ffBh  Bo��Bw��B��B�  B�  B�  B�  B�  B�33B�  B���B�  B�  B���B�33B�33B�  B�  B�33B�  B�  B���B�  B�33B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  C   C  C  C�fC�fC
  C�C�C  C  C  C  C  C  C�fC  C �C"�C$�C&  C'�fC)�fC+�fC-�fC/�fC2  C4  C6  C8�C:�C<  C=�fC@  CB  CD�CF  CH  CJ  CL  CN  CP  CR  CS�fCV  CX  CZ  C\  C^�C`�Cb�Cd�Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct�Cv�Cx  Cz  C|  C}�fC�  C�  C�  C��3C�  C��C��C�  C�  C�  C�  C�  C��3C�  C�  C��C��C�  C��3C�  C�  C�  C��C�  C�  C�  C�  C��C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C�  C�  C��3C��3C��3C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C�  C�  C�  C�  C�  C��C��C�  C�  C��C��3C�  C�  C�  C�  C��3C�  C�  C�  C�  C��C��C�  C�  C��C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C��3C�  C��C�  C�  C��C�  C�  D   D y�D  D� D��D� D  D� D  D� D  D� DfD� D��D� D  D� D	  D	� D
  D
y�D
��D� DfD�fDfD�fDfD� D��Dy�D��D� D  D� D��D� D��Dy�D  D�fD  Dy�D  D� D  D� DfD�fD  D� D  D� DfD� D  Dy�D  D�fD  D� D��D� D fD � D!  D!� D!��D"� D#fD#� D$  D$y�D$��D%� D&fD&� D&��D'� D(  D(y�D)  D)�fD*fD*y�D*��D+�fD,fD,�fD-fD-�fD.  D.� D/  D/y�D0  D0� D1  D1� D1��D2� D3  D3� D4fD4� D5  D5� D6  D6� D7fD7�fD8  D8� D9  D9� D:  D:� D;  D;�fD<fD<�fD=fD=� D=��D>y�D>��D?� D@  D@� DA  DA�fDB  DBy�DC  DC� DD  DD� DE  DE� DF  DF�fDG  DGy�DH  DH� DI  DIy�DJ  DJ�fDK  DK� DL  DL� DM  DM�fDN  DN� DO  DO� DPfDP�fDQ  DQy�DQ��DR� DS  DS� DT  DT� DU  DU� DVfDV� DW  DWy�DX  DX�fDYfDY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^�fD_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd�fDefDe�fDf  Df� DgfDg�fDhfDh� Dh��Di� DjfDj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dny�Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dy��D�,�D�ffD���D���D�6fD�l�D��fD���D��D�ffD���D��3D�)�D�L�Dڠ D�� D��D�FfD�fD�	�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��A��A�(�A�+A�-A�-A�/A�1'A�1'A�1'A�1'A�1'A�1'A�33A�5?A�7LA�7LA�5?A�5?A�5?A�7LA�+A�oA�z�A��A̬A�5?A��mA˃Aʇ+A�
=A�|�AȬA� �A�1A��A��A�1A���A��PA�XA��A��FA�A���A�;dA��7A���A�p�A�5?A�+A��A��hA���A�%A��!A���A��hA�-A��!A��mA�p�A���A��A��7A��TA���A��A�bNA��jA�^5A�&�A���A�33A�oA��A��hA�33A��jA�A���A���A���A���A�(�A���A���A�hsA�jA��A��HA�33A�A�&�A��jA�VA�`BA�v�A��A��A�;dA�M�A�p�A���A�5?A��
A�jA���A�&�A�l�A�x�A���A��^A��PA�A�A��wA���A�G�A��-A�XA�M�A���A�A���A�;dA��A��A�^5A��A�ĜA�A���A�n�A���A�%A��A�=qA��DA���A�-A�Az��Aw�Av�Atn�Ar(�An��Am�Alr�Ai�-Ah�Af9XAdĜAb�yAa�^A`��A^A\��A[�AY�7AX�uAW�AU�
AS��AR��AQ��AQ`BAQVAP�9AO\)ALffAK��AK
=AJ9XAG�AFr�AF  AE?}AC"�AB��AB��A@��A?��A?O�A=33A<1A9�
A7C�A5��A4-A3�hA2�A1��A0��A0A�A/��A/oA-A,9XA+�A+%A*r�A*JA)�-A)XA(�jA(A�A(-A&��A%XA$�jA#��A#�7A#%A"z�A!�mA ffAƨA��Al�A�HAE�A�A�wA(�A�FAO�AĜA�mAx�A��A�^A�/A�
A�/A�^A�HA��A1'A;dAjA��A��A��A`BA��A�RAQ�A
M�A	VA�9AQ�A�hA�9Ar�A��A�yA1Ar�AƨAK�@���@�O�@�K�@�j@�+@��!@��@�Ĝ@��@��@��@���@�/@웦@�(�@�w@�dZ@�ȴ@�-@���@�&�@���@��/@��@�!@�h@���@۝�@ڏ\@�x�@��/@��`@��@���@֧�@�5?@թ�@�X@�G�@�/@Ұ!@�Z@Η�@���@ʗ�@���@�/@Ȭ@�A�@ǥ�@�ȴ@��@�`B@�^5@�A�@��@Ǿw@ǝ�@ǝ�@�K�@���@��#@�`B@�O�@ċD@���@��\@�v�@���@���@���@��h@��`@�z�@�bN@�V@��`@��@��/@��/@��`@��`@���@��@�O�@�O�@�x�@��h@��@���@��D@��\@�Q�@���@��+@��D@���@��;@�M�@��@��u@�b@��F@�+@�v�@��T@�7L@�ƨ@���@��@��@���@�@�%@�I�@��P@�t�@���@�@���@��+@�^5@��T@���@���@�@���@�x�@��h@�O�@���@���@�1'@�ƨ@� �@�  @��y@�ȴ@�ȴ@�$�@��^@�O�@�%@���@�?}@�p�@�hs@��@�A�@�z�@��@��u@��@�A�@�1@��w@�
=@��R@��\@�=q@�@���@�G�@�?}@��D@� �@��F@��@�"�@�%@�(�@��@��@��w@���@���@�t�@�"�@�ȴ@��R@�v�@���@���@��!@��@�O�@��@���@��@���@��D@��@�Q�@� �@�I�@�O�@���@���@�C�@�C�@���@���@���@�~�@�~�@��+@��+@�n�@�M�@�$�@�@��T@�@�@���@���@��h@��@�p�@�?}@��@��@�  @��w@���@��@��F@��@�V@y��@qG�@iG�@`��@W�@L�@E�T@@b@:~�@3S�@,�@%�@ �@o@K�@t�@�P@�@K�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111A��A�(�A�+A�-A�-A�/A�1'A�1'A�1'A�1'A�1'A�1'A�33A�5?A�7LA�7LA�5?A�5?A�5?A�7LA�+A�oA�z�A��A̬A�5?A��mA˃Aʇ+A�
=A�|�AȬA� �A�1A��A��A�1A���A��PA�XA��A��FA�A���A�;dA��7A���A�p�A�5?A�+A��A��hA���A�%A��!A���A��hA�-A��!A��mA�p�A���A��A��7A��TA���A��A�bNA��jA�^5A�&�A���A�33A�oA��A��hA�33A��jA�A���A���A���A���A�(�A���A���A�hsA�jA��A��HA�33A�A�&�A��jA�VA�`BA�v�A��A��A�;dA�M�A�p�A���A�5?A��
A�jA���A�&�A�l�A�x�A���A��^A��PA�A�A��wA���A�G�A��-A�XA�M�A���A�A���A�;dA��A��A�^5A��A�ĜA�A���A�n�A���A�%A��A�=qA��DA���A�-A�Az��Aw�Av�Atn�Ar(�An��Am�Alr�Ai�-Ah�Af9XAdĜAb�yAa�^A`��A^A\��A[�AY�7AX�uAW�AU�
AS��AR��AQ��AQ`BAQVAP�9AO\)ALffAK��AK
=AJ9XAG�AFr�AF  AE?}AC"�AB��AB��A@��A?��A?O�A=33A<1A9�
A7C�A5��A4-A3�hA2�A1��A0��A0A�A/��A/oA-A,9XA+�A+%A*r�A*JA)�-A)XA(�jA(A�A(-A&��A%XA$�jA#��A#�7A#%A"z�A!�mA ffAƨA��Al�A�HAE�A�A�wA(�A�FAO�AĜA�mAx�A��A�^A�/A�
A�/A�^A�HA��A1'A;dAjA��A��A��A`BA��A�RAQ�A
M�A	VA�9AQ�A�hA�9Ar�A��A�yA1Ar�AƨAK�@���@�O�@�K�@�j@�+@��!@��@�Ĝ@��@��@��@���@�/@웦@�(�@�w@�dZ@�ȴ@�-@���@�&�@���@��/@��@�!@�h@���@۝�@ڏ\@�x�@��/@��`@��@���@֧�@�5?@թ�@�X@�G�@�/@Ұ!@�Z@Η�@���@ʗ�@���@�/@Ȭ@�A�@ǥ�@�ȴ@��@�`B@�^5@�A�@��@Ǿw@ǝ�@ǝ�@�K�@���@��#@�`B@�O�@ċD@���@��\@�v�@���@���@���@��h@��`@�z�@�bN@�V@��`@��@��/@��/@��`@��`@���@��@�O�@�O�@�x�@��h@��@���@��D@��\@�Q�@���@��+@��D@���@��;@�M�@��@��u@�b@��F@�+@�v�@��T@�7L@�ƨ@���@��@��@���@�@�%@�I�@��P@�t�@���@�@���@��+@�^5@��T@���@���@�@���@�x�@��h@�O�@���@���@�1'@�ƨ@� �@�  @��y@�ȴ@�ȴ@�$�@��^@�O�@�%@���@�?}@�p�@�hs@��@�A�@�z�@��@��u@��@�A�@�1@��w@�
=@��R@��\@�=q@�@���@�G�@�?}@��D@� �@��F@��@�"�@�%@�(�@��@��@��w@���@���@�t�@�"�@�ȴ@��R@�v�@���@���@��!@��@�O�@��@���@��@���@��D@��@�Q�@� �@�I�@�O�@���@���@�C�@�C�@���@���@���@�~�@�~�@��+@��+@�n�@�M�@�$�@�@��T@�@�@���@���@��h@��@�p�@�?}@��@��@�  @��w@���@��@��F@��@�V@y��@qG�@iG�@`��@W�@L�@E�T@@b@:~�@3S�@,�@%�@ �@o@K�@t�@�P@�@K�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oB�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�BhBuB�B�B�B�B�B�B�B!�B,B=qBO�BR�B_;BgmBhsBm�B�B�%B��B�3B�}BȴBɺB��B��B��B��B��B��B��B�
B�B�B�B�B�#B�B��B��B��B��B��B��B��B��B��B��B��BȴBƨBÖB�}B�qB�XB�9B�!B�B��B��B��B��B�uB�VB�+B�B~�Bx�Bp�BffB`BBO�BE�B:^B6FB/B#�B�BDBB��B�B�mB�5B��BŢB�LB��B��B��B�uB�JB|�Bs�BhsBT�BD�B.B�BJBB
��B
�B
�/B
�dB
��B
��B
��B
��B
�bB
�%B
� B
w�B
q�B
_;B
YB
?}B
'�B
hB
B	�B	�NB	��B	ƨB	ĜB	�dB	�FB	�B	��B	��B	�VB	�%B	u�B	hsB	\)B	N�B	D�B	:^B	0!B	&�B	!�B	�B	�B	�B	hB	DB	B	  B��B��B�B�B�B�sB�ZB�BB�/B�B��B��BɺBŢB�}B�'B�B��B��B��B��B��B��B��B��B��B��B��B��B��B��B�{B�uB�uB�hB�VB�7B�B�B� B~�B}�B|�By�By�Bz�Bz�By�Bx�Bw�Bw�Bv�Bv�Bu�Bt�Bs�Br�Bp�Bo�Bk�Bl�BjBgmBffBffBdZBbNBbNBaHB`BB_;B_;B^5B]/B[#BYB\)B[#BYBW
BT�BVBW
BXBW
BS�BQ�BK�BG�BC�BB�BA�B>wB<jB:^B8RB5?B2-B2-B33B33B33B5?B6FB7LB8RB;dB>wB?}B?}B@�BC�BG�BG�BJ�BJ�BH�BI�BT�B[#B`BBbNBhsBp�Bs�Bt�Bs�Br�Bp�Bq�Bk�BbNBYBW
BW
BVBT�BS�BR�BS�BVB[#Bk�By�B|�B}�B}�B}�B� B�B�B�DB�\B�oB�PB�B~�B|�Bz�Bx�Bx�By�Bz�B~�B�1B�PB��B��B��B��B��B��B��B��B��B��B��B�9B�LB�FB�B��B��B��B��B��B��B��B��B��B��B��B�B�B�3B�9B�XB�XB�XB�XB�dB�}BBŢBƨBȴB��B��B��B��B��B��B��B�B�B�B�)B�;B�ZB�sB�B�B��B��B��B��B��B	B	B		7B	DB	VB	oB	�B	�B	�B	�B	�B	!�B	$�B	%�B	&�B	'�B	)�B	,B	1'B	49B	5?B	6FB	:^B	:^B	=qB	A�B	B�B	C�B	B�B	B�B	A�B	>wB	>wB	B�B	E�B	L�B	O�B	R�B	S�B	VB	W
B	XB	ZB	]/B	aHB	bNB	e`B	e`B	e`B	ffB	gmB	gmB	iyB	k�B	l�B	m�B	r�B	}�B	�B	�=B	�VB	�\B	�\B	�\B	�\B	�bB	�hB	�hB	�hB	�oB	�{B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�?B	��B	�mB	��B
%B
DB
�B
�B
'�B
/B
5?B
>wB
E�B
N�B
R�B
XB
]/B
aHB
gmB
l�B
q�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B �B"�B%�B1'B?}BN�BZB\)BbNBhsBk�Bq�B�B�\B��B�dBǮB��B��B��B��B��B��B�B�B�B�B�B�#B�;B�NB�HB�HB�#B�B�B�
B�B�B�B��B��B�B�BB��B��B��BĜBÖB��B�XB�LB�-B�'B��B��B��B��B��B�DB�%B�+B~�Bx�Bo�Br�BYBO�BB�BA�B;dB/B"�BuB	7B��B��B�B�mB�)B��B��B�3B��B��B��B��B�B~�Bx�BdZBVB:^B-BuB
=B%B+B
��B
ƨB
�'B
��B
��B
��B
��B
�JB
�1B
�B
~�B
jB
q�B
ZB
9XB
�B
{B
%B	��B	�
B	��B	�B	ĜB	ŢB	�FB	�B	��B	��B	��B	~�B	t�B	ffB	VB	M�B	F�B	;dB	.B	(�B	�B	�B	�B	�B	�B	
=B	+B	B	%B��B��B��B�B�mB�ZB�sB�)B�B�)B��B��B��B�jB�?B�B�B�B��B��B��B��B�B��B��B��B��B��B��B��B��B��B��B��B�hB�1B�%B�B�B�B�B�B}�B|�B}�B}�B|�Bz�B|�B� By�By�By�By�Bv�Bv�Bw�Br�Bt�Br�Bn�Bl�BiyBhsBiyBhsBgmBhsBe`BbNBaHBaHBaHBffBcTB^5B]/B]/BZBYB^5B^5B_;BS�BW
BQ�BG�BL�BJ�BJ�BB�B?}B>wB=qB5?B:^B2-B5?B5?B5?B7LB8RB9XB8RB;dB@�B?}B?}B@�BC�BG�BM�BJ�BJ�BH�BM�BW
B\)B`BBe`BhsBp�Bu�Bt�Bt�Br�By�By�Br�BjBYBW
BZBXBW
BVBR�BVBW
B[#BgmBy�B}�B~�B}�B~�B� B�B�%B�PB�\G�O�B�PB�oB�B|�Bz�Bz�Bx�Bz�Bz�B}�B�7B�PB��B��B��B��B��B��B��B��B��B��B��B�RB�^B�FB�?B�B�'B��B��B��B��B��B��B��B�B�B�B�'B�3B�9B�jB�dB�^B�^B�jBBĜBŢBƨBȴB��B��B��B��B��B��B��B�B�#B�B�)B�;B�`B�yB�B�B��B��B��B��B	  B	B	%B	
=B	JB	VB	hB	�B	�B	�B	�B	�B	!�B	$�B	%�B	'�B	(�B	+B	,B	1'B	5?B	5?B	6FB	;dB	;dB	>wB	C�B	C�B	D�B	C�B	B�B	G�B	@�B	>wB	B�B	E�B	L�B	O�B	S�B	T�B	W
B	W
B	YB	ZB	]/B	bNB	e`B	e`B	gmB	e`B	ffB	gmB	gmB	iyB	l�B	l�B	l�B	o�B	{�B	�B	�=B	�VB	�bB	�\B	�\B	�\B	�bB	�hB	�hB	�hB	�oB	�{B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�?B	��B	�mB	��B
%B
DB
�B
�B
'�B
/B
5?B
>wB
E�B
N�B
R�B
XB
]/B
aHB
gmB
l�B
q�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
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
<u<���<�C�<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<�o<D��<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<�t�<#�
<#�
<#�
<49X<D��<49X<49X<#�
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
<T��<#�
<49X<�o<�o<�C�<D��<T��<#�
<#�
<#�
<�1<�h<49X<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<T��<49X<ě�<���<�C�<D��<u<�t�<���<#�
<D��<�C�<#�
<u<49X<49X<#�
<#�
<u<#�
<D��<#�
<#�
<#�
<D��<49X<#�
<#�
<#�
<#�
<#�
<49X<�o<#�
<#�
<#�
<�o<#�
<#�
<#�
<49X<#�
<#�
<49X<#�
<#�
<T��<49X<u<�o<49X<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
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
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
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
PRES            TEMP            PSAL            PRES_ADJUSTED = PRES - dP                                                                                                                                                                                                                                       none                                                                                                                                                                                                                                                            PSAL_ADJUSTED = sw_salt( sw_cndr(PSAL,TEMP,PRES), TEMP, PRES_ADJUSTED ) + CTM                                                                                                                                                                                   dP =0 dbar.                                                                                                                                                                                                                                                     none                                                                                                                                                                                                                                                            CTM: alpha=0.141, tau=6.68s                                                                                                                                                                                                                                     Pressures adjusted by using pressure offset at the sea surface. The quoted error is manufacturer specified accuracy in dbar.                                                                                                                                    The quoted error is manufacturer specified accuracy with respect to ITS-90 at time of laboratory calibration.                                                                                                                                                   No significant salinity drift detected. Salinity adjusted for effects of pressure adjustment. Conductivity cell thermal mass correction (CTM) applied to measurements taken in Cp mode. The quoted error is max[0.01, sqrt(OWerr^2+CTMerr^2)] in PSS-78.        201201031447042012010314470420120103144704  AO  ARGQ                                                                        20111130141131  QCP$                G�O�G�O�G�O�FFBFE           AO  ARGQ                                                                        20111130141131  QCF$                G�O�G�O�G�O�0               UW  ARSQUWQC    WOD & nearby Argo as visual check                               20120103144704  IP                  G�O�G�O�G�O�                