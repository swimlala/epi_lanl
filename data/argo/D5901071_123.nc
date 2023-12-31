CDF      
      	DATE_TIME         STRING2       STRING4       STRING8       STRING16      STRING32       STRING64   @   	STRING256         N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       D2011-11-30 AOML 2.2 creation; 2015-12-13T22:40:25Z UW 3.1 conversion   
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
_FillValue        G�O�     �  U�   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                    ]�   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     	valid_min         �      	valid_max         B      
resolution        :�o   C_format      %9.3f      FORTRAN_format        F9.3f      
_FillValue        G�O�     �  _�   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                    gx   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     units         degree_Celsius     
resolution        :�o   C_format      %9.3f      FORTRAN_format        F9.3f      
_FillValue        G�O�     �  ix   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    	valid_min         @      	valid_max         B$     
resolution        :�o   C_format      %9.3f      FORTRAN_format        F9.3f      
_FillValue        G�O�     �  qp   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                    yh   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    	valid_min         @      	valid_max         B$     
resolution        :�o   C_format      %9.3f      FORTRAN_format        F9.3f      
_FillValue        G�O�     �  {h   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                    �`   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     units         psu    
resolution        :�o   C_format      %9.3f      FORTRAN_format        F9.3f      
_FillValue        G�O�     �  �`   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  0  �X   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                    ��   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                    ��   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                    ��   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  ,  ��   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                    ��   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                    ��   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                    ��   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                    ��   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  @  ��   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                    �   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                    �   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    �   HISTORY_START_PRES                    	long_name          Start pressure action applied on   units         decibar    
_FillValue        G�O�        �(   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    units         decibar    
_FillValue        G�O�        �,   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�        �0   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    �4Argo profile    3.1 1.2 19500101000000  5901071 US ARGO PROJECT                                                 Stephen Riser                                                   PRES            TEMP            PSAL               {A   AO  20111130141953  20190522121827  1727_5046_123                   2C  D   APEX                            2143                            040306                          846 @Դ��f��1   @Դ�q� @7�V��d333331   GPS     Primary sampling: mixed [deeper than nominal 980dbar: discrete; nominal 980dbar to surface: 2dbar-bin averaged]                                                                                                                                                    A   A   A   @33@y��@�  A   A   A@  A`  A�  A�  A���A���A���A�  A�  A���B ffB  B  B  B��B(  B0  B7��B@  BH  BP  BW��B`  Bh  BpffBxffB�  B���B�  B�  B�33B�  B�  B�33B�  B�  B�  B�  B�  B�  B�  B�  B�33B�  B���B�  B�33B�  B���B�  B�33B�33B�  B�  B���B���B���B���B���C�fC  C�fC�fC	�fC  C  C  C�C�C�C  C�fC�fC  C   C"  C#�fC&  C(  C*  C+�fC.  C0  C2  C4  C5�fC8  C:  C;�fC=�fC@  CB  CD  CF  CH  CJ  CL  CM�fCP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Co�fCr  Ct  Cu�fCx  Cz�C|  C~  C�  C�  C�  C��3C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C��C�  C�  C�  C�  C��C�  C�  C��C�  C�  C��3C��3C�  C��C�  C�  C�  C��C�  C�  C�  C�  C��C�  C�  C�  C��3C�  C�  C�  C��C�  C��3C�  C��C�  C�  C�  C��C�  C�  C��3C�  C��C�  C�  C��3C��3C�  C��C��C�  C��3C�  C�  C��C�  C�  C�  C��3C�  C�  C�  C��3C�  C�  C��3C�  C�  C�  C�  C�  C�  C�  C��3C��3C��3C�  C�  C��C�  C�  C�  C��C�  C�  C�  C�  C�  C��3C��3C��3C��3C�  C��C�  C�  C�  C�  C�  C��C�  C��3C�  C�  C�  C��C�  C�  C�  D   D �fD  D� DfD� D  D� DfD� D  D� D  D�fD  D� D  D� D	fD	� D
  D
� D  D� D  D� D  D� D  D� D��D� D  D� D  D� D  D�fD  Dy�D  D� D  D�fD  D� D  D� D  D� D  D� D  D� D  D� D��Dy�D��D� DfD� D  D� D��D � D!fD!� D"  D"� D#  D#y�D$  D$� D$��D%y�D&  D&�fD'fD'� D'��D(y�D)  D)� D*  D*�fD+  D+� D,  D,� D,��D-� D.fD.� D/  D/�fD0  D0� D1  D1�fD2  D2y�D2��D3y�D3��D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:fD:� D;  D;� D<fD<� D<��D=� D>fD>�fD?  D?� D@  D@� DA  DA� DB  DB�fDC  DCy�DD  DD� DE  DEy�DE��DFy�DG  DG� DH  DH� DI  DI� DJfDJ� DJ��DK� DL  DL� DM  DM� DN  DN�fDO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS�fDT  DTy�DU  DU� DVfDV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D[��D\� D]  D]� D]��D^� D_fD_�fD`fD`�fDafDa� Da��Dby�Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� DhfDh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dt� Dy�fD�,�D�\�D���D��fD�33D�Y�D�� D�� D�&fD�\�D���D���D��D�l�Dڙ�D��3D�3D�Y�D�fD��3111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  @��@�  @�33A��A!��AA��Aa��A���A���A���A���A���A���A���A�B ��BffBffBffB   B(ffB0ffB8  B@ffBHffBPffBX  B`ffBhffBp��Bx��B�33B�  B�33B�33B�ffB�33B�33B�ffB�33B�33B�33B�33B�33B�33B�33B�33B�ffB�33B�  B�33B�ffB�33B�  B�33B�ffB�ffB�33B�33B�  B�  B�  B�  C   C  C�C  C  C
  C�C�C�C33C33C33C�C  C  C�C �C"�C$  C&�C(�C*�C,  C.�C0�C2�C4�C6  C8�C:�C<  C>  C@�CB�CD�CF�CH�CJ�CL�CN  CP�CR�CT�CV�CX�CZ�C\�C^�C`�Cb�Cd�Cf�Ch�Cj�Cl�Cn�Cp  Cr�Ct�Cv  Cx�Cz33C|�C~�C��C��C��C�  C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C�  C�  C��C��C��C��C��C��C��C��C��C��C��C��C��C��C�  C��C��C��C��C��C�  C��C��C��C��C��C��C��C��C�  C��C��C��C��C�  C�  C��C��C��C��C�  C��C��C��C��C��C��C�  C��C��C��C�  C��C��C�  C��C��C��C��C��C��C��C�  C�  C�  C��C��C��C��C��C��C��C��C��C��C��C��C�  C�  C�  C�  C��C��C��C��C��C��C��C��C��C�  C��C��C��C��C��C��C��D fD ��DfD�fD�D�fDfD�fD�D�fDfD�fDfD��DfD�fDfD�fD	�D	�fD
fD
�fDfD�fDfD�fDfD�fDfD�fD  D�fDfD�fDfD�fDfD��DfD� DfD�fDfD��DfD�fDfD�fDfD�fDfD�fDfD�fDfD�fD  D� D  D�fD�D�fDfD�fD   D �fD!�D!�fD"fD"�fD#fD#� D$fD$�fD%  D%� D&fD&��D'�D'�fD(  D(� D)fD)�fD*fD*��D+fD+�fD,fD,�fD-  D-�fD.�D.�fD/fD/��D0fD0�fD1fD1��D2fD2� D3  D3� D4  D4�fD5fD5�fD6fD6�fD7fD7�fD8fD8�fD9fD9�fD:�D:�fD;fD;�fD<�D<�fD=  D=�fD>�D>��D?fD?�fD@fD@�fDAfDA�fDBfDB��DCfDC� DDfDD�fDEfDE� DF  DF� DGfDG�fDHfDH�fDIfDI�fDJ�DJ�fDK  DK�fDLfDL�fDMfDM�fDNfDN��DOfDO�fDPfDP�fDQfDQ�fDRfDR�fDSfDS��DTfDT� DUfDU�fDV�DV�fDWfDW�fDXfDX�fDYfDY�fDZfDZ�fD[fD[�fD\  D\�fD]fD]�fD^  D^�fD_�D_��D`�D`��Da�Da�fDb  Db� DcfDc�fDdfDd�fDefDe�fDffDf�fDgfDg�fDh�Dh�fDifDi�fDjfDj�fDkfDk�fDlfDl�fDmfDm�fDnfDn�fDofDo�fDpfDp�fDqfDq�fDrfDr�fDsfDs�fDtfDt�fDy��D�0 D�` D�� D���D�6fD�\�D��3D��3D�)�D�` D���D�� D��D�p Dڜ�D��fD�fD�\�D�D��f111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  @��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��A�l�A�p�A�l�A�r�A�r�A�r�A�t�A�t�A�t�A�v�A�t�A�v�A�v�A�v�A�x�A�z�A�z�A�|�A�z�A�|�A��A��A��A��A��+A��7A��DA��DA��PA��PA��DA��DA��PA��PA��\A��PA��A��+A��+A��A�ffA�\)A�M�A�?}A�ĜA���A�ĜA�oA���A�1'A��FA�5?A��
A�$�A�JA��mA��hA��\A�Q�A�33A��!A��A��A���A�~�A���A�ƨA�A��A�;dA���A���A�7LA��`A��A��A�l�A�A�jA���A���A��DA�p�A�dZA�XA�E�A��uA���A��RA���A�A��-A�\)A��A���A��-A��mA��A�ƨA�&�A�A�z�A��PA�v�A���A��7A�C�A�
=A���A�9XA�n�A���A~�yA{��Az-Ay`BAxn�Av�uAu�mAt{Arv�AqG�Ao`BAmAk�AhĜAfZAd�Adr�AdE�Ac�
Ab��Aa��A`jA^�`A]A\  AZ�AY�AU�^AS
=ARbAQ33AQAP�\AO��AO�AM�;AK��AK"�AI�AHE�AGƨAGK�AG�AE��ADbAC�PAB�DAA��AAoA@�!A?A>ĜA=O�A<n�A;��A;��A;"�A8�yA7�A5��A4��A3�^A1��A/
=A-�wA,��A,�A+t�A+"�A*�A*ĜA*~�A)�^A)S�A(�9A'��A&��A%��A%+A$��A$ffA#�TA"�A"��A!�PA��A{A5?A`BA
=A��AA��A��A��At�AĜA1AK�A~�A��AG�A�uA5?A1'A�AdZA��AZAJA�PA��A��A��A;dA�9A^5A �A��A�;AVA�
A\)A�HA=qA/A ȴ@��@��@���@�J@��@�  @�ȴ@�^5@��7@�I�@���@�E�@���@��@�n�@�\)@땁@��H@�@�S�@�dZ@�"�@��@�1'@�V@��@�b@��@��`@�
=@ۥ�@�5?@ج@�j@���@��@��@��@�&�@�/@�G�@Ձ@�x�@�X@��;@�@�X@�9X@�~�@͉7@�V@�  @�ƨ@˅@ʰ!@�-@�J@ɉ7@���@�1'@�t�@�S�@ƸR@��@�"�@�?}@�dZ@��@���@�b@��F@�|�@��@�V@���@���@�J@�O�@�O�@�I�@���@��y@���@�`B@���@��@� �@�(�@�b@��@��m@��;@���@�33@�~�@�@��9@���@���@���@���@��m@�K�@�o@��y@���@�M�@�{@���@�7L@��@�&�@���@�j@�I�@�+@�@�ȴ@�v�@�-@�J@�J@�J@��#@��-@��h@�`B@�&�@��`@�j@��;@�S�@��\@��@��@��@�`B@�?}@��@��j@���@�bN@�(�@�(�@�b@���@��;@��@�"�@��R@�V@��@�{@�J@�J@�@�{@���@��@� �@�dZ@��H@���@�v�@�V@��#@��@��u@�z�@�j@�9X@��w@��@�\)@�;d@�"�@�o@��H@���@�^5@���@���@��7@��@�`B@�O�@�%@���@��@��@��
@�ƨ@��@���@�S�@�@��H@��!@�n�@�M�@��@�`B@�?}@�Q�@��@��@��m@��F@��@��F@���@�dZ@�o@���@�~�@�5?@�{@���@���@��7@�G�@��@��/@�Ĝ@��@� �@�1@��@��m@��
@��;@��;@�K�@��H@���@���@�~�@�-@�{@��@��^@�`B@��`@���@�9X@���@���@�l�@�C�@�33@�O�@x�`@o�@g\)@]O�@Tz�@N��@H�u@@��@;C�@3S�@.��@+33@'+@"��@ff@�\@V@�7@��@	x�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  A�l�A�p�A�l�A�r�A�r�A�r�A�t�A�t�A�t�A�v�A�t�A�v�A�v�A�v�A�x�A�z�A�z�A�|�A�z�A�|�A��A��A��A��A��+A��7A��DA��DA��PA��PA��DA��DA��PA��PA��\A��PA��A��+A��+A��A�ffA�\)A�M�A�?}A�ĜA���A�ĜA�oA���A�1'A��FA�5?A��
A�$�A�JA��mA��hA��\A�Q�A�33A��!A��A��A���A�~�A���A�ƨA�A��A�;dA���A���A�7LA��`A��A��A�l�A�A�jA���A���A��DA�p�A�dZA�XA�E�A��uA���A��RA���A�A��-A�\)A��A���A��-A��mA��A�ƨA�&�A�A�z�A��PA�v�A���A��7A�C�A�
=A���A�9XA�n�A���A~�yA{��Az-Ay`BAxn�Av�uAu�mAt{Arv�AqG�Ao`BAmAk�AhĜAfZAd�Adr�AdE�Ac�
Ab��Aa��A`jA^�`A]A\  AZ�AY�AU�^AS
=ARbAQ33AQAP�\AO��AO�AM�;AK��AK"�AI�AHE�AGƨAGK�AG�AE��ADbAC�PAB�DAA��AAoA@�!A?A>ĜA=O�A<n�A;��A;��A;"�A8�yA7�A5��A4��A3�^A1��A/
=A-�wA,��A,�A+t�A+"�A*�A*ĜA*~�A)�^A)S�A(�9A'��A&��A%��A%+A$��A$ffA#�TA"�A"��A!�PA��A{A5?A`BA
=A��AA��A��A��At�AĜA1AK�A~�A��AG�A�uA5?A1'A�AdZA��AZAJA�PA��A��A��A;dA�9A^5A �A��A�;AVA�
A\)A�HA=qA/A ȴ@��@��@���@�J@��@�  @�ȴ@�^5@��7@�I�@���@�E�@���@��@�n�@�\)@땁@��H@�@�S�@�dZ@�"�@��@�1'@�V@��@�b@��@��`@�
=@ۥ�@�5?@ج@�j@���@��@��@��@�&�@�/@�G�@Ձ@�x�@�X@��;@�@�X@�9X@�~�@͉7@�V@�  @�ƨ@˅@ʰ!@�-@�J@ɉ7@���@�1'@�t�@�S�@ƸR@��@�"�@�?}@�dZ@��@���@�b@��F@�|�@��@�V@���@���@�J@�O�@�O�@�I�@���@��y@���@�`B@���@��@� �@�(�@�b@��@��m@��;@���@�33@�~�@�@��9@���@���@���@���@��m@�K�@�o@��y@���@�M�@�{@���@�7L@��@�&�@���@�j@�I�@�+@�@�ȴ@�v�@�-@�J@�J@�J@��#@��-@��h@�`B@�&�@��`@�j@��;@�S�@��\@��@��@��@�`B@�?}@��@��j@���@�bN@�(�@�(�@�b@���@��;@��@�"�@��R@�V@��@�{@�J@�J@�@�{@���@��@� �@�dZ@��H@���@�v�@�V@��#@��@��u@�z�@�j@�9X@��w@��@�\)@�;d@�"�@�o@��H@���@�^5@���@���@��7@��@�`B@�O�@�%@���@��@��@��
@�ƨ@��@���@�S�@�@��H@��!@�n�@�M�@��@�`B@�?}@�Q�@��@��@��m@��F@��@��F@���@�dZ@�o@���@�~�@�5?@�{@���@���@��7@�G�@��@��/@�Ĝ@��@� �@�1@��@��m@��
@��;@��;@�K�@��H@���@���@�~�@�-@�{@��@��^@�`B@��`@���@�9X@���@���@�l�@�C�@�33@�O�@x�`@o�@g\)@]O�@Tz�@N��@H�u@@��@;C�@3S�@.��@+33@'+@"��@ff@�\@V@�7@��@	x�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  ;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oBw�Bw�Bv�Bw�Bw�Bw�Bw�Bw�Bw�Bw�Bw�Bw�Bw�Bw�Bw�Bv�Bv�Bv�Bv�Bv�Bw�Bw�Bw�Bw�Bw�Bw�Bw�Bw�Bw�Bw�Bw�Bw�Bw�Bw�Bw�Bv�Bv�Bv�Bu�Bu�Bt�Bs�Bq�Bn�Bk�Bm�Bo�B}�By�By�Bz�B� B�B�B�B~�B{�Bv�Bu�Bt�Bt�Bn�BhsBiyBhsBdZBbNB_;BQ�B?}B<jB5?B%�B�B\B+B��B��B�B�#B��BɺBǮBŢBÖB��B�FB��B��B�oB�Bv�Bq�Bl�BgmBaHBR�BA�B1'B�BB
�B
�)B
�jB
��B
�hB
�DB
�B
|�B
n�B
R�B
2-B
"�B
oB
%B
B	��B	�B	�sB	�)B	��B	ƨB	�^B	�-B	�-B	��B	��B	��B	��B	��B	��B	��B	�\B	�7B	�B	{�B	s�B	k�B	cTB	T�B	I�B	F�B	B�B	@�B	=qB	9XB	49B	.B	&�B	!�B	�B	�B	�B	�B	oB	PB	1B	B	B��B��B��B��B�B�B�sB�`B�NB�/B�B��B��BŢB��B�dB�LB�9B�-B�!B�B�B�B�B�B��B��B��B��B��B��B��B��B��B��B��B�{B�hB�JB�1B�=B�+B�%B�B�B� B|�Bz�By�Bx�Bx�Bw�Bv�Bv�Bu�Bu�Bt�Bs�Bq�Bm�Bp�Bn�Bl�BffBYBQ�BO�BM�BK�BK�BJ�BI�BG�BF�BF�BC�BA�B@�B?}B?}BE�BC�BE�BF�BG�BG�BH�BF�BE�BE�BG�BI�BVB\)B^5B\)BZBZB[#B_;BaHBe`BdZBcTBaHB_;B]/BYBVBP�BN�BP�BT�BVBcTBiyBk�Bk�Bk�Bk�Bl�Bu�Bv�Bu�Bx�Bx�Bx�Bw�B{�B{�Bz�B{�B{�Bz�B~�B�B�B�B�B�B�B�B�B�B~�B�B�1B�DB�JB�JB�PB�\B�hB��B��B��B��B��B��B��B��B�B�-B�3B�FB�jBŢB��B��B��B��B��B��B��B�B�B�5B�TB�mB�B�B��B��B��B��B��B��B��B	  B	B	B	B	+B	DB	JB	VB	oB	�B	�B	�B	�B	 �B	"�B	&�B	'�B	(�B	+B	,B	.B	0!B	5?B	9XB	=qB	A�B	D�B	G�B	I�B	L�B	N�B	Q�B	T�B	XB	ZB	YB	ZB	ZB	ZB	[#B	_;B	`BB	aHB	bNB	cTB	cTB	bNB	bNB	cTB	dZB	e`B	gmB	iyB	k�B	m�B	p�B	r�B	v�B	|�B	�B	�B	�B	�B	�+B	�1B	�7B	�=B	�DB	�DB	�JB	�\B	�hB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�B	�B	�B	�'B	�3B	�FB	�RB	�dB	�dB	�jB	�qB	�}B	B	ÖB	ĜB	ĜB	ĜB	ƨB	ǮB	ȴB	ɺB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�#B	�)B	�/B	�5B	�5B	�;B	�;B	�fB	��B
B
PB
�B
%�B
+B
2-B
9XB
>wB
G�B
K�B
P�B
VB
ZB
_;B
cTB
ffB
k�B
q�B
s�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  Bw�Bw�Bv�Bw�Bw�Bw�Bw�Bw�Bw�Bw�Bw�Bw�Bw�Bw�Bw�Bv�Bv�Bv�Bv�Bv�Bw�Bw�Bw�Bw�Bw�Bw�Bw�Bw�Bw�Bw�Bw�Bw�Bw�Bw�Bw�Bv�Bv�Bv�Bu�Bv�Bu�Bt�Bt�Bw�B}�B~�B�1B�7B�B�=B�1B�%B�DB�\B�VB�PB�By�By�B}�B�Bt�Bp�Bp�Bs�Bn�Bl�BjB\)BG�BJ�BD�B49B!�B�BbBB  B��B�fB��B��BȴBǮBǮB��B��B�FB��B��B�VBz�Bt�Bo�Bm�Bm�BaHBR�BD�B(�B�B  B
��B
��B
��B
��B
�\B
�=B
�DB
�1B
r�B
D�B
5?B
�B
PB
	7B
B	��B	��B	�fB	�B	��B	ƨB	ÖB	��B	�LB	��B	��B	��B	��B	��B	��B	��B	�{B	�DB	�1B	~�B	w�B	z�B	dZB	O�B	K�B	D�B	D�B	C�B	@�B	=qB	:^B	/B	.B	$�B	�B	�B	�B	�B	�B	PB	DB	1B	B	  B	  B��B��B�B�B�yB�yB�B�NB�B��B��B��B��B��B�dB�LB�9B�'B�!B�B�B�!B�B�B�B��B��B��B��B��B��B��B��B��B��B��B�uB�\B�=B�=B�=B�1B�1B�1B�B� B}�B~�B}�B{�By�By�Bw�Bu�Bv�Bx�B{�Bs�Br�Bs�Bu�B^5BS�BS�BP�BM�BM�BL�BL�BM�BM�BJ�BG�BF�BF�BB�BC�BG�BF�BH�BJ�BK�BK�BJ�BJ�BI�BH�BL�BL�B\)BgmBiyBgmB\)BZB[#B`BBaHBk�Bk�Bl�BaHBdZBffB\)B^5BP�BT�BVBXB]/BcTBiyBk�Bk�Bk�Bk�Bl�Bv�Bx�B{�B~�B{�Bx�B}�B~�B}�B}�B|�B}�Bz�B�B�B�B�%B�+B�B�+B�1B�7B�=B�%B�1B�PB�\B�VB�PB�\B�oB�hB��B��B��B��B��B��B�B��B�!B�-B�?B�LB�qBŢB��B��B��B��B��B��B��B�B�5B�5B�TB�B�B�B��B��B��B��B��B��B��B	  B	B	B	B		7B	JB	\B	VB	uB	�B	�B	�B	�B	 �B	#�B	'�B	(�B	)�B	,B	-B	0!B	2-B	7LB	;dB	=qB	B�B	D�B	G�B	I�B	M�B	N�B	Q�B	T�B	XB	ZB	YB	ZB	[#B	[#B	\)B	`BB	`BB	bNB	bNB	cTB	cTB	bNB	bNB	cTB	dZB	hsB	iyB	jB	l�B	m�B	q�B	r�B	x�B	|�B	�B	�B	�B	�B	�1B	�1B	�7B	�=B	�DB	�JB	�PB	�bB	�oB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�B	�B	�!B	�-B	�3B	�FB	�RB	�dB	�dB	�jB	�qB	��B	B	ÖB	ŢB	ĜB	ŢB	ƨB	ǮB	ɺB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�
B	�B	�)B	�)B	�5B	�5B	�5B	�;B	�;B	�fB	��B
B
PB
�B
%�B
+B
2-B
8RB
>wB
G�B
J�B
P�B
VB
ZB
_;B
bNB
ffB
k�B
q�B
s�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  <#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<�t�<�C�<ě�<49X<49X<�o<T��<#�
<#�
<T��<T��<e`B<#�
<#�
<#�
<#�
<u<#�
<#�
<#�
<49X<#�
<#�
<49X<#�
<#�
<e`B<u<e`B<#�
<49X<#�
<#�
<#�
<�o<49X<#�
<#�
<#�
<#�
<#�
<#�
<49X<D��<D��<e`B<#�
<#�
<#�
<#�
<#�
<D��<e`B<�C�<���<�C�<���<�t�<�`B<ě�<#�
<#�
<#�
<#�
<e`B<���<��<�t�<�t�<#�
<#�
<#�
<49X<#�
<D��<#�
<#�
<e`B<D��<�C�<u<e`B<#�
<#�
<#�
<#�
<#�
<#�
<#�
<49X<#�
<D��<49X<D��<�j<u<#�
<#�
<#�
<#�
<#�
<#�
<#�
<D��<#�
<D��<#�
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
<u<D��<#�
<#�
<#�
<u<�o<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<49X<e`B<D��<49X<#�
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
<e`B<#�
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
<49X<49X<49X<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
PRES            TEMP            PSAL            PRES_ADJUSTED = PRES - dP                                                                                                                                                                                                                                       none                                                                                                                                                                                                                                                            PSAL_ADJUSTED = sw_salt( sw_cndr(PSAL,TEMP,PRES), TEMP, PRES_ADJUSTED ) + CTM                                                                                                                                                                                   dP =-0.1 dbar.                                                                                                                                                                                                                                                  none                                                                                                                                                                                                                                                            CTM: alpha=0.141, tau=6.68s                                                                                                                                                                                                                                     Pressures adjusted by using pressure offset at the sea surface. The quoted error is manufacturer specified accuracy in dbar.                                                                                                                                    The quoted error is manufacturer specified accuracy with respect to ITS-90 at time of laboratory calibration.                                                                                                                                                   No significant salinity drift detected. Salinity adjusted for effects of pressure adjustment. Conductivity cell thermal mass correction (CTM) applied to measurements taken in Cp mode. The quoted error is max[0.01, sqrt(OWerr^2+CTMerr^2)] in PSS-78.        201201031447172012010314471720120103144717  AO  ARGQ                                                                        20111130141953  QCP$                G�O�G�O�G�O�FFBFE           AO  ARGQ                                                                        20111130141953  QCF$                G�O�G�O�G�O�0               UW  ARSQUWQC    WOD & nearby Argo as visual check                               20120103144717  IP                  G�O�G�O�G�O�                