CDF      
      	DATE_TIME         STRING2       STRING4       STRING8       STRING16      STRING32       STRING64   @   	STRING256         N_PROF        N_PARAM       N_LEVELS     N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       D2011-11-30 AOML 2.2 creation; 2015-12-28T23:01:56Z UW 3.1 conversion   
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
_FillValue        G�O�   axis      Z          9�   PRES_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                   A�   PRES_ADJUSTED            
      	   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     units         decibar    	valid_min                	valid_max         F;�    
resolution        =���   C_format      %7.1f      FORTRAN_format        F7.1f      
_FillValue        G�O�       C�   PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                   K�   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     units         decibar    
resolution        =���   C_format      %7.1f      FORTRAN_format        F7.1f      
_FillValue        G�O�       M�   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     	valid_min         �      	valid_max         B      
resolution        :�o   C_format      %9.3f      FORTRAN_format        F9.3f      
_FillValue        G�O�       U�   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                   ]�   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     	valid_min         �      	valid_max         B      
resolution        :�o   C_format      %9.3f      FORTRAN_format        F9.3f      
_FillValue        G�O�       _�   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                   g�   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     units         degree_Celsius     
resolution        :�o   C_format      %9.3f      FORTRAN_format        F9.3f      
_FillValue        G�O�       i�   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    	valid_min         @      	valid_max         B$     
resolution        :�o   C_format      %9.3f      FORTRAN_format        F9.3f      
_FillValue        G�O�       q�   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                   z   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    	valid_min         @      	valid_max         B$     
resolution        :�o   C_format      %9.3f      FORTRAN_format        F9.3f      
_FillValue        G�O�       |   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                   �   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     units         psu    
resolution        :�o   C_format      %9.3f      FORTRAN_format        F9.3f      
_FillValue        G�O�       �   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  0  �$   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                    �T   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                    �T   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                    �T   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  ,  �T   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                    ��   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                    ��   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                    ��   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                    ��   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  @  ��   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                    ��   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                    ��   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    ��   HISTORY_START_PRES                    	long_name          Start pressure action applied on   units         decibar    
_FillValue        G�O�        ��   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    units         decibar    
_FillValue        G�O�        ��   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�        ��   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    � Argo profile    3.1 1.2 19500101000000  5901072 US ARGO PROJECT                                                 Stephen Riser                                                   PRES            TEMP            PSAL               AA   AO  20111130144152  20190522121829  1728_5048_065                   2C  D   APEX                            2142                            040306                          846 @��J��o�1   @��K����@5��n���b����S�1   GPS     Primary sampling: mixed [deeper than nominal 990dbar: discrete; nominal 990dbar to surface: 2dbar-bin averaged]                                                                                                                                                    A   A   A   @�  @�  A   A   A@  A`  A�  A�  A�  A�  A�33A�  A�  A�  B   B  B  B  B   B(  B0  B8  B?��BH  BP  BW��B`  Bh  Bp  BxffB�  B�  B���B���B�  B�33B�  B���B���B���B�  B�  B�  B�33B�  B���B�  B�  B�33B�  B�  B�  B�  B�  B�  B�  B�  B�33B�  B�  B�  B�  C   C  C  C  C  C
  C  C  C  C�fC�fC�fC  C  C�fC  C   C"�C$�C&  C(  C*�C,  C.  C0�C2  C4  C6  C8  C:  C<�C>�C@  CB  CD  CF  CG�fCJ  CL  CN  CP  CR�CT�CV  CW�fCZ  C\  C^  C`  Cb  Cd  Cf�Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C��C�  C�  C��3C��3C��3C��3C��3C��3C�  C�  C�  C�  C��C��C�  C��3C�  C�  C��C��C�  C��3C��3C��3C��3C�  C��C�  C�  C�  C��C��C�  C�  C�  C�  C��3C�  C��C�  C�  C��C�  C�  C��C�  C�  C��3C�  C�  C�  C�  C��3C��3C�  C��C�  C�  C��C��C��C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C�  C�  C��3C�  C�  C�  C�  C��C��C�  C��3C�  C��C�  C�  C�  C�  C�  C��3C�  C�  C�  C��C�  C�  C�  C�  C�  C��3C��3C�  C�  C�  C�  C��C�  C��3C�  C��C�  C��3C�  C��C�  D   D y�D  D� D  D� D  Dy�D  D�fD  D� D  D� D  D� DfD� D	  D	� D	��D
� D  Dy�D  D�fD  D� D  D� D  Dy�D  D� D��D� DfD� D  D�fD  Dy�D  D� D  D� D  Dy�D  D�fD  D� DfD� D  Dy�D  D� D  D� D��D� DfD� D   D y�D ��D!� D"fD"� D#  D#y�D$  D$�fD%fD%� D&  D&� D&��D'� D(  D(�fD)fD)� D*  D*� D+  D+y�D,  D,� D,��D-y�D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D3��D4� D5  D5� D6  D6y�D6��D7y�D8  D8� D8��D9y�D9��D:y�D:��D;y�D;��D<y�D<��D=y�D=��D>� D?  D?� D@  D@� DA  DA� DA��DB� DC  DC� DD  DD�fDE  DEy�DF  DF� DG  DG�fDH  DHy�DI  DI� DJ  DJ�fDKfDK�fDL  DLy�DM  DM� DN  DN� DO  DOy�DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DUfDU�fDVfDV� DV��DW� DX  DX�fDYfDY� DZ  DZ� D[  D[�fD\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Ddy�De  De� Df  Df� Dg  Dg� Dh  Dh�fDi  Diy�Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dny�Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dt� Du  Du� Dv  Dv� Dw  Dws3Dy�fD�	�D�9�D�� D�� D�  D�)�D�|�D�� D�  D�  D��Dǹ�D��fD�fD��D�fD���D��D�L�D�f11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 @fff@�33@�33A��A9��AY��Ay��A���A���A���A�  A���A���A���A���BffBffBffBffB&ffB.ffB6ffB>  BFffBNffBV  B^ffBfffBnffBv��B~ffB�33B�  B�  B�33B�ffB�33B�  B�  B�  B�33B�33B�33B�ffB�33B�  B�33B�33B�ffB�33B�33B�33B�33B�33B�33B�33B�33B�ffB�33B�33B�33B�33B�33C��C��C��C��C	��C��C��C��C� C� C� C��C��C� C��C��C!�3C#�3C%��C'��C)�3C+��C-��C/�3C1��C3��C5��C7��C9��C;�3C=�3C?��CA��CC��CE��CG� CI��CK��CM��CO��CQ�3CS�3CU��CW� CY��C[��C]��C_��Ca��Cc��Ce�3Cg��Ci��Ck��Cm��Co��Cq��Cs��Cu��Cw��Cy��C{��C}��C��C���C���C���C���C���C���C���C�ٚC���C���C�ٚC���C���C�� C�� C�� C�� C�� C�� C���C���C���C���C�ٚC�ٚC���C�� C���C���C�ٚC�ٚC���C�� C�� C�� C�� C���C�ٚC���C���C���C�ٚC�ٚC���C���C���C���C�� C���C�ٚC���C���C�ٚC���C���C�ٚC���C���C�� C���C���C���C���C�� C�� C���C�ٚC���C���C�ٚC�ٚC�ٚC�ٚC���C���C���C���C���C���C���C���C���C�� C���C���C�� C���C���C���C���C�ٚC�ٚC���C�� C���C�ٚC���C���C���C���C���C�� C���C���C���C�ٚC���C���C���C���C���C�� C�� C���C���C���C���C�ٚC���C�� C���C�ٚC���C�� C���C�ٚC���C���D ` D �fDffD�fDffD�fD` D�fDl�D�fDffD�fDffD�fDffD��DffD�fD	ffD	� D
ffD
�fD` D�fDl�D�fDffD�fDffD�fD` D�fDffD� DffD��DffD�fDl�D�fD` D�fDffD�fDffD�fD` D�fDl�D�fDffD��DffD�fD` D�fDffD�fDffD� DffD��DffD�fD ` D � D!ffD!��D"ffD"�fD#` D#�fD$l�D$��D%ffD%�fD&ffD&� D'ffD'�fD(l�D(��D)ffD)�fD*ffD*�fD+` D+�fD,ffD,� D-` D-�fD.ffD.�fD/ffD/�fD0ffD0�fD1ffD1�fD2ffD2�fD3ffD3� D4ffD4�fD5ffD5�fD6` D6� D7` D7�fD8ffD8� D9` D9� D:` D:� D;` D;� D<` D<� D=` D=� D>ffD>�fD?ffD?�fD@ffD@�fDAffDA� DBffDB�fDCffDC�fDDl�DD�fDE` DE�fDFffDF�fDGl�DG�fDH` DH�fDIffDI�fDJl�DJ��DKl�DK�fDL` DL�fDMffDM�fDNffDN�fDO` DO�fDPffDP�fDQffDQ�fDRffDR�fDSffDS�fDTffDT��DUl�DU��DVffDV� DWffDW�fDXl�DX��DYffDY�fDZffDZ�fD[l�D[�fD\ffD\�fD]ffD]�fD^ffD^�fD_ffD_�fD`ffD`�fDaffDa�fDbffDb�fDcffDc�fDd` Dd�fDeffDe�fDfffDf�fDgffDg�fDhl�Dh�fDi` Di�fDjffDj�fDkffDk�fDlffDl�fDmffDm�fDn` Dn�fDoffDo�fDpffDp�fDqffDq�fDrffDr�fDsffDs�fDtffDt�fDuffDu�fDvffDv�fDwY�Dy��D���D�,�D�s3D��3D��3D��D�p D��3D��3D�3D�  DǬ�D��D�	�D� Dਗ਼D�� D��D�@ D�	�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 @��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��A���A���A��A�JA��-A��hA��A�v�A�jA�dZA�^5A�\)A�VA�Q�A�O�A�O�A�K�A�K�A�I�A�E�A�A�A�A�A�A�A�I�A�dZA��A��PA���A��jA�|�A��A�|�A��A��#A��9A���A��\A�p�A�XA�&�A��/A��-A���A��wA���A��`A��`A��;A��RA�hsA�33A��A��yA���A�`BA�K�A��A�|�A�$�A��;A��
A�ȴA�VA���A�C�A��;A�bA��
A���A�A�^5A��;A���A�ĜA�%A�(�A��-A���A�hsA��A�`BA���A�hsA���A��+A�9XA���A�ȴA�(�A�`BA���A�^5A��A���A�r�A���A�?}A��!A�^5A�K�A��;A��RA���A��A�A�
=A��A���A�
=A�=qA�dZA��A���A�
=A���A���A�JA��A�JA��#A�A�VA�$�A�jA�VA�JA}+Az��Ay�#Aw�At�`Ar{Ap  Am+AjjAg��Af{Ac��A`�A^�`A]%AZ��AXv�AT��AR�AQ�PAP~�AOx�ANE�AL�9AI�#AG��AFv�AE��ADZACdZAB�+AA�A?�hA>1A<��A;�;A:�/A9�mA9&�A8-A7�7A6�A4VA3�A2v�A1�;A0�A/hsA.�DA-l�A,v�A+K�A*5?A)oA'�mA&��A&A%��A%G�A$��A#��A"M�A!�A �9A (�Ax�AZA��A��A�\A�#A7LA9XA�-A\)A%A�\A1'A�A
=A9XAS�A9XA�FA��A  A9XA$�A��A?}A�DA��A�A��AA\)A�\A�A
ZA	;dA~�A1A�PA1'AA(�A��AG�A��AjA$�A�A`BA E�@���@�9X@�I�@�p�@��j@��;@��@�{@�@�&�@�E�@�v�@�%@�9X@��@�`B@�P@�-@�h@�r�@�ff@ܼj@��H@ّh@�9X@���@�V@�1@�-@��/@϶F@�ff@�7L@�1'@�S�@�=q@�G�@�b@�M�@���@��m@�v�@���@���@�K�@�%@���@�"�@�~�@��-@�/@�j@�ƨ@�33@��@�J@��;@���@�-@�&�@���@�9X@�@�ff@�5?@�@���@�bN@�33@�=q@��^@�?}@���@���@���@�-@��7@�V@��9@��F@�@��+@��@�+@�+@�@��+@��^@��@��@�Z@�
=@��\@�5?@���@�%@���@��@��@�K�@�@��@��!@��+@��R@�@�J@���@��@�@�$�@�5?@�5?@���@�O�@�p�@�@���@�`B@�V@��@�Q�@�1'@�1@��@��@��
@���@���@��F@���@�b@�bN@�9X@��@���@�\)@�33@�o@�@�+@�S�@�ƨ@��@��m@��
@�ƨ@���@��P@�t�@�K�@�
=@�ȴ@�~�@�=q@��@��-@�x�@�%@��9@�r�@�I�@� �@��;@�|�@�C�@�;d@�+@��@�ȴ@���@��+@�ff@�-@��^@�x�@�G�@��`@�j@�(�@�b@���@��@��
@��F@��P@�S�@�33@�@��R@�~�@�5?@��@���@�p�@�O�@�&�@��@���@��9@���@��@�j@�bN@�Z@�I�@�1'@���@��F@��F@��@���@���@�l�@�33@�o@��@��+@��@�hs@�?}@��@��`@�Ĝ@��u@�j@�I�@�9X@� �@�1@��w@�C�@�ȴ@���@��+@�n�@�5?@���@���@���@��h@�O�@�/@��@���@��9@�j@�I�@�I�@��@���@��@�\)@�"�@�^5@~�R@tj@m�-@c�m@[dZ@Q&�@J��@Dz�@>5?@6$�@1&�@*�H@'K�@$�D@��@7L@1@��@ƨ@�`11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 A���A���A��A�JA��-A��hA��A�v�A�jA�dZA�^5A�\)A�VA�Q�A�O�A�O�A�K�A�K�A�I�A�E�A�A�A�A�A�A�A�I�A�dZA��A��PA���A��jA�|�A��A�|�A��A��#A��9A���A��\A�p�A�XA�&�A��/A��-A���A��wA���A��`A��`A��;A��RA�hsA�33A��A��yA���A�`BA�K�A��A�|�A�$�A��;A��
A�ȴA�VA���A�C�A��;A�bA��
A���A�A�^5A��;A���A�ĜA�%A�(�A��-A���A�hsA��A�`BA���A�hsA���A��+A�9XA���A�ȴA�(�A�`BA���A�^5A��A���A�r�A���A�?}A��!A�^5A�K�A��;A��RA���A��A�A�
=A��A���A�
=A�=qA�dZA��A���A�
=A���A���A�JA��A�JA��#A�A�VA�$�A�jA�VA�JA}+Az��Ay�#Aw�At�`Ar{Ap  Am+AjjAg��Af{Ac��A`�A^�`A]%AZ��AXv�AT��AR�AQ�PAP~�AOx�ANE�AL�9AI�#AG��AFv�AE��ADZACdZAB�+AA�A?�hA>1A<��A;�;A:�/A9�mA9&�A8-A7�7A6�A4VA3�A2v�A1�;A0�A/hsA.�DA-l�A,v�A+K�A*5?A)oA'�mA&��A&A%��A%G�A$��A#��A"M�A!�A �9A (�Ax�AZA��A��A�\A�#A7LA9XA�-A\)A%A�\A1'A�A
=A9XAS�A9XA�FA��A  A9XA$�A��A?}A�DA��A�A��AA\)A�\A�A
ZA	;dA~�A1A�PA1'AA(�A��AG�A��AjA$�A�A`BA E�@���@�9X@�I�@�p�@��j@��;@��@�{@�@�&�@�E�@�v�@�%@�9X@��@�`B@�P@�-@�h@�r�@�ff@ܼj@��H@ّh@�9X@���@�V@�1@�-@��/@϶F@�ff@�7L@�1'@�S�@�=q@�G�@�b@�M�@���@��m@�v�@���@���@�K�@�%@���@�"�@�~�@��-@�/@�j@�ƨ@�33@��@�J@��;@���@�-@�&�@���@�9X@�@�ff@�5?@�@���@�bN@�33@�=q@��^@�?}@���@���@���@�-@��7@�V@��9@��F@�@��+@��@�+@�+@�@��+@��^@��@��@�Z@�
=@��\@�5?@���@�%@���@��@��@�K�@�@��@��!@��+@��R@�@�J@���@��@�@�$�@�5?@�5?@���@�O�@�p�@�@���@�`B@�V@��@�Q�@�1'@�1@��@��@��
@���@���@��F@���@�b@�bN@�9X@��@���@�\)@�33@�o@�@�+@�S�@�ƨ@��@��m@��
@�ƨ@���@��P@�t�@�K�@�
=@�ȴ@�~�@�=q@��@��-@�x�@�%@��9@�r�@�I�@� �@��;@�|�@�C�@�;d@�+@��@�ȴ@���@��+@�ff@�-@��^@�x�@�G�@��`@�j@�(�@�b@���@��@��
@��F@��P@�S�@�33@�@��R@�~�@�5?@��@���@�p�@�O�@�&�@��@���@��9@���@��@�j@�bN@�Z@�I�@�1'@���@��F@��F@��@���@���@�l�@�33@�o@��@��+@��@�hs@�?}@��@��`@�Ĝ@��u@�j@�I�@�9X@� �@�1@��w@�C�@�ȴ@���@��+@�n�@�5?@���@���@���@��h@�O�@�/@��@���@��9@�j@�I�@�I�@��@���@��@�\)@�"�@�^5@~�R@tj@m�-@c�m@[dZ@Q&�@J��@Dz�@>5?@6$�@1&�@*�H@'K�@$�D@��@7L@1@��@ƨ@�`11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 ;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oB,B+B)�B(�B(�B+B,B,B-B,B-B.B.B.B/B/B0!B0!B1'B0!B0!B1'B2-B6FBC�BZBhsB~�B�XB��BhB�B�B"�B'�B+B+B,B.B2-B;dBD�BP�B[#B_;BdZBgmBo�By�B{�B�B��B��B��B�9B�RB��BĜBÖBǮB��B��B��BɺBĜBǮBŢB�!B�uB|�BffBcTBe`BffBL�B(�B �B�B�B�B�B�BoB{B�B�B�B�B�B\BDB+B1B  B��B��B�B�B�B�sB�NB��B��B�-B��B��B�Bm�BaHBT�BF�B6FB'�B�BB
�sB
ȴB
�9B
��B
�uB
�B
jB
R�B
B�B
/B
�B
1B	��B	��B	�BB	��B	��B	��B	�JB	�JB	y�B	y�B	ffB	XB	K�B	B�B	33B	!�B	uB	B��B��B�B�fB�/BɺB��B�^B�LB�B�B��B��B��B��B��B��B��B��B��B��B��B��B��B�oB�oB�\B�7B�+B�B�B~�B~�Bz�Bz�Bw�Bv�Bv�Bv�Bw�Bu�Br�Bn�Bm�Bk�BjBhsBgmBhsBffBe`BcTBaHB`BB_;B^5B\)B[#BZBZBXBZBT�BS�BS�BYBe`Bn�By�B� B�B�B�B� B� B~�B}�B{�Bv�Bq�Bn�Bl�BjBiyBe`B_;B_;B_;B^5B]/B]/B]/B^5B_;B`BB[#BW
BO�BG�BD�BD�BA�B@�BA�BA�B<jB6FB33B33B2-B1'B1'B/B-B,B-B+B-B,B.B0!B2-B2-B5?B49B5?B7LB:^B:^B:^B<jB=qB=qB@�BA�BD�BF�BF�BI�BK�BQ�BS�BR�BT�BVBYB\)B]/B\)B\)BaHBcTBgmBjBm�Br�Bs�Bw�Bx�Bx�Bz�B}�B~�B�B�B�%B�+B�%B�+B�+B�7B�DB�\B��B��B��B��B��B��B��B��B��B��B�B�B�B�B�B�B�B�'B�9B�RB�XB�dB�wB��BƨBɺB��B��B�B�B�BB�`B�yB�B�B��B��B��B	1B	DB	PB	oB	�B	�B	�B	#�B	)�B	,B	1'B	8RB	<jB	?}B	E�B	I�B	M�B	P�B	Q�B	Q�B	S�B	W
B	\)B	aHB	iyB	l�B	q�B	t�B	u�B	v�B	w�B	x�B	y�B	z�B	{�B	~�B	�B	�B	�%B	�7B	�=B	�=B	�JB	�PB	�\B	�\B	�bB	�uB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�!B	�3B	�?B	�FB	�FB	�LB	�LB	�RB	�XB	�dB	�jB	�qB	��B	B	ĜB	ŢB	ŢB	ǮB	ȴB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�
B	�B	�B	�5B	�5B	�5B	�;B	�BB	�BB	�HB	�NB	�NB	�TB	�NB	�ZB	�`B	�fB	�mB	�mB	�sB	�sB	�yB	�yB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B
B
JB
�B
 �B
(�B
2-B
9XB
>wB
C�B
J�B
P�B
W
B
[#B
^5B
cTB
iyB
n�B
r�B
w�B
z�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 B,B-B.B,B)�B,B-B-B-B,B-B.B.B.B/B/B0!B0!B1'B0!B0!B1'B2-B5?BB�BZBgmB� B�jBB�B�B �B$�B(�B,B-B.B1'B5?B=qBD�BO�BZB^5BdZBhsBq�B|�B}�B�B��B��B�B�FB�wBĜBȴBƨBȴB��B�#B�)B��B��B��B�B�}B��B�PBl�BffBl�B|�B^5B.B"�B!�B"�B$�B&�B�B�B�B$�B'�B)�B#�B�B�BoBhBhB%BB��B��B�B�B�B�B�5B��B�jB�FB��B�bBv�Bk�B`BBQ�BA�B2-B+B�B
��B
�B
��B
�FB
��B
��B
y�B
`BB
S�B
?}B
1'B
�B
%B
+B	�B	��B	�XB	�B	��B	��B	�7B	�DB	y�B	gmB	\)B	T�B	G�B	:^B	 �B	DB	B��B��B��B�B�BɺBB��B�LB�?B�3B�9B�B��B��B��B��B��B�B�B�B�B��B��B��B��B�uB�\B�PB�DB�1B�1B�B�B� B{�Bz�Bz�B}�B}�B{�Bu�Bs�Bp�Bq�Bq�Bq�Bq�Bo�BjBiyBgmBdZBbNBaHB`BB_;B^5B`BB^5BaHB\)BW
BS�BW
Be`Bp�B}�B�B�1B�7B�+B�B�B�B�B�B�By�Bt�Bq�Bp�Bs�Bm�Be`BcTBbNBbNB`BB`BB`BBdZBhsBhsBffBdZBYBJ�BH�BH�BD�BB�BH�BM�BH�B;dB8RB:^B8RB1'B6FB33B33B49B49B+B2-B,B49B6FB7LB9XB:^B9XB5?B<jB>wB>wB:^B@�BC�B=qBE�BA�BI�BJ�BK�BP�BS�BQ�BVBVBT�BYBYB_;B`BB\)BaHBaHBgmBjBn�Bo�Bu�Bs�By�Bx�Bz�B}�B}�B�B�B�B�1B�=B�7B�=B�+B�DB�DB�hB��B��B��B��B��B��B��B��B��B��B�B�-B�-B�!B�B�B�!B�'B�9B�jB�jB�jB�}B��BǮBɺB��B��B�
B�B�BB�`B�yB�B�B��B��B��B		7B	JB	PB	{B	�B	�B	�B	#�B	)�B	,B	2-B	8RB	<jB	?}B	E�B	I�B	M�B	P�B	Q�B	Q�B	S�B	W
B	\)B	aHB	iyB	k�B	q�B	t�B	v�B	w�B	w�B	y�B	z�B	{�B	|�B	~�B	�B	�B	�+B	�=B	�DB	�=B	�PB	�PB	�bB	�bB	�hB	�uB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�'B	�9B	�FB	�LB	�FB	�RB	�RB	�RB	�XB	�dB	�qB	�qB	��B	ÖB	ĜB	ŢB	ƨB	ȴB	ɺB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�)B	�;B	�;B	�;B	�BB	�BB	�HB	�HB	�NB	�NB	�ZB	�TB	�`B	�fB	�mB	�sB	�sB	�sB	�yB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B
B
JB
�B
 �B
)�B
33B
9XB
>wB
D�B
J�B
P�B
W
B
\)B
_;B
cTB
jB
o�B
s�B
w�B
z�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 <#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<D��<�o<u<�9X<�o<#�
<#�
<#�
<�9X<�C�<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<e`B<T��<49X<#�
<T��<�C�<D��<#�
<#�
<49X<49X<49X<#�
<�C�<���<�1<u<T��<�o<e`B<�1<u<T��<�C�<�o<�C�<e`B<#�
<�o<���<���<u<��
<�t�<�C�<u<�C�<���<u<u<�t�<��
<�j<T��<#�
<#�
<#�
<49X<u<���<e`B<#�
<#�
<#�
<#�
<#�
<#�
<e`B<D��<#�
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
<49X<T��<#�
<#�
<#�
<#�
<#�
<#�
<#�
<D��<D��<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
PRES            TEMP            PSAL            PRES_ADJUSTED = PRES - dP                                                                                                                                                                                                                                       none                                                                                                                                                                                                                                                            PSAL_ADJUSTED = sw_salt( sw_cndr(PSAL,TEMP,PRES), TEMP, PRES_ADJUSTED ) + CTM                                                                                                                                                                                   dP =0.4 dbar.                                                                                                                                                                                                                                                   none                                                                                                                                                                                                                                                            CTM: alpha=0.141, tau=6.68s                                                                                                                                                                                                                                     Pressures adjusted by using pressure offset at the sea surface. The quoted error is manufacturer specified accuracy in dbar.                                                                                                                                    The quoted error is manufacturer specified accuracy with respect to ITS-90 at time of laboratory calibration.                                                                                                                                                   No significant salinity drift detected. Salinity adjusted for effects of pressure adjustment. Conductivity cell thermal mass correction (CTM) applied to measurements taken in Cp mode. The quoted error is max[0.01, sqrt(OWerr^2+CTMerr^2)] in PSS-78.        201201101452162012011014521620120110145216  AO  ARGQ                                                                        20111130144152  QCP$                G�O�G�O�G�O�FFBFE           AO  ARGQ                                                                        20111130144152  QCF$                G�O�G�O�G�O�0               UW  ARSQUWQC    WOD & nearby Argo as visual check                               20120110145216  IP                  G�O�G�O�G�O�                