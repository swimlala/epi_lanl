CDF      
      	DATE_TIME         STRING2       STRING4       STRING8       STRING16      STRING32       STRING64   @   	STRING256         N_PROF        N_PARAM       N_LEVELS     N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       D2011-11-30 AOML 2.2 creation; 2015-12-28T23:01:58Z UW 3.1 conversion   
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
_FillValue                    � Argo profile    3.1 1.2 19500101000000  5901072 US ARGO PROJECT                                                 Stephen Riser                                                   PRES            TEMP            PSAL               GA   AO  20111130144230  20190522121829  1728_5048_071                   2C  D   APEX                            2142                            040306                          846 @��.L��1   @��.��?�@6���l�D�b�z�G�1   GPS     Primary sampling: mixed [deeper than nominal 990dbar: discrete; nominal 990dbar to surface: 2dbar-bin averaged]                                                                                                                                                    A   A   A   @���@�  @���A   A@  A`  A�  A�  A�  A�  A�  A�  A�  A�  B   B  B  B  B ffB(  B0  B8  B@  BH  BP  BX  B`  Bh  Bp  Bx  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  C   C  C  C  C  C
  C  C  C  C  C  C  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Cs�fCv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C�  C�  C�  C�  C�  C�  C��C��C��C�  C�  C�  C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D ��D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� DfD� D  D� D  D� D  D� D  D�fD  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)�fD*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm�fDn  Dn� Do  Do� Dp  Dp� Dq  Dq�fDr  Dr� Ds  Ds� Dt  Dt� Du  Du� Dv  Dv� Dw  Dws3Dy��D��3D�6fD�|�D���D��3D�)�D�l�D�ɚD�� D�  D�s3Dǩ�D�ٚD�fDډ�D๚D��3D� D�@ D�c311111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 @�  @�33@�  A��A9��AY��Ay��A���A���A���A���A���A���A���A���BffBffBffB��B&ffB.ffB6ffB>ffBFffBNffBVffB^ffBfffBnffBvffB~ffB�33B�33B�33B�33B�33B�33B�33B�33B�33B�33B�33B�33B�33B�33B�33B�33B�33B�33B�33B�33B�33B�33B�33B�33B�33B�33B�33B�33B�33B�33B�33B�33C��C��C��C��C	��C��C��C��C��C��C��C��C��C��C��C��C!��C#��C%��C'��C)��C+��C-��C/��C1��C3��C5��C7��C9��C;��C=��C?��CA��CC��CE��CG��CI��CK��CM��CO��CQ��CS��CU��CW��CY��C[��C]��C_��Ca��Cc��Ce��Cg��Ci��Ck��Cm��Co��Cq��Cs� Cu��Cw��Cy��C{��C}��C��C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C�� C���C���C���C���C���C���C�ٚC�ٚC�ٚC���C���C���C�� C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���D ffD � DffD�fDffD�fDffD�fDffD�fDffD�fDffD�fDffD�fDffD�fD	ffD	�fD
ffD
�fDffD�fDffD�fDffD�fDffD�fDffD�fDffD�fDffD�fDffD�fDffD�fDffD�fDffD�fDffD��DffD�fDffD�fDffD�fDffD�fDl�D�fDffD�fDffD�fDffD�fDffD�fD ffD �fD!ffD!�fD"ffD"�fD#ffD#�fD$ffD$�fD%ffD%�fD&ffD&�fD'ffD'�fD(ffD(�fD)l�D)�fD*ffD*�fD+ffD+�fD,ffD,�fD-ffD-�fD.ffD.�fD/ffD/�fD0ffD0�fD1ffD1�fD2ffD2�fD3ffD3�fD4ffD4�fD5ffD5�fD6ffD6�fD7ffD7�fD8ffD8�fD9ffD9�fD:ffD:�fD;ffD;�fD<ffD<�fD=ffD=�fD>ffD>�fD?ffD?�fD@ffD@�fDAffDA�fDBffDB�fDCffDC�fDDffDD�fDEffDE�fDFffDF�fDGffDG�fDHffDH�fDIffDI�fDJffDJ�fDKffDK�fDLffDL�fDMffDM�fDNffDN�fDOffDO�fDPffDP�fDQffDQ�fDRffDR�fDSffDS�fDTffDT�fDUffDU�fDVffDV�fDWffDW�fDXffDX�fDYffDY�fDZffDZ�fD[ffD[�fD\ffD\�fD]ffD]�fD^ffD^�fD_ffD_�fD`ffD`�fDaffDa�fDbffDb�fDcffDc�fDdffDd�fDeffDe�fDfffDf�fDgffDg�fDhffDh�fDiffDi�fDjffDj�fDkffDk�fDlffDl�fDml�Dm�fDnffDn�fDoffDo�fDpffDp�fDql�Dq�fDrffDr�fDsffDs�fDtffDt�fDuffDu�fDvffDv�fDwY�Dy�3D��fD�)�D�p D�� D��fD��D�` D���D��3D�3D�ffDǜ�D���D�	�D�|�D��D��fD�3D�33D�Vf11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 @��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��AƲ-AƲ-AƲ-AƲ-Aƴ9Aƴ9Aư!AƩ�AƧ�AƩ�AƩ�AƧ�AƩ�AƬA���A���AƋDA���A�5?A�bA���A���Aħ�A��A�1A�A�jA�I�A�1A���A���A��-A�Q�A�A�G�A��A��#A�;dA��RA�|�A��wA�A�A��+A��^A��PA�E�A�+A�A�A�7LA��9A��yA���A���A��A�  A��PA��A�dZA�JA��A�^5A�ȴA��uA��hA�ĜA���A���A�XA��uA���A��PA��PA��!A� �A�
=A�z�A�M�A�5?A�1A��!A�&�A��A�v�A�oA��uA�A�A�jA�I�A���A��DA���A�p�A�1'A�VA��wA��DA��jA�VA��jA��TA�bA�A�(�A�l�A���A�`BA�-A��A�I�A��A�ȴA�`BA�S�A�1A}dZA{O�Az1Ax��Av�`AuS�As��Aq��An��Am�#AlQ�Aj��Ah�\Af�!Ae"�AdZA^E�A[?}AX�!AWp�AU��ATv�ASAP(�AL5?AJ�jAH��AG|�AGVAE�
ACƨAB�AA�A@��A?S�A=ƨA=33A<�uA;|�A: �A8z�A8(�A7�^A7dZA6z�A4�`A4 �A3G�A2(�A0��A/�A-33A*��A)l�A(��A'"�A%��A%A#�TA"��A!C�A r�A�;At�A��AXA��A��A�A"�A1'AhsA�RAVA�A��AĜA�A��AK�A��A1'A��AXAbNAp�AĜA�AK�A��AA
��A
A�A	ƨA	;dA1A;dAM�A��A/A��A�!A�A�uA��AXA%A v�@�\)@��@�\)@�~�@��u@�@�1@�o@��@� �@�M�@�x�@�9@�|�@�V@��@�|�@�t�@�l�@�  @ް!@��@�?}@�bN@�C�@١�@��/@�I�@׍P@�=q@�V@ԛ�@�z�@�C�@�O�@�9X@��y@�?}@̴9@̃@�bN@�z�@��@˅@ʧ�@�-@ɉ7@ț�@�33@��@š�@�7L@ģ�@�z�@�I�@� �@�C�@�@�J@���@�X@��`@��
@���@��#@�hs@��/@��@���@���@�I�@�  @��
@��@�\)@��@��@�?}@���@�1'@���@�K�@�"�@�@��@�ȴ@�~�@���@��`@���@���@���@�A�@� �@��@�1@���@�+@�x�@�\)@�v�@�x�@�%@���@��
@�@��-@�Q�@��@�5?@���@��@��@��y@�n�@���@���@��h@�`B@�Ĝ@���@��D@��D@�bN@�1'@��F@�33@�@��@��!@�ff@�E�@�^5@�5?@�{@��^@�G�@�/@��@�Ĝ@�z�@�(�@��;@���@�33@��!@���@��\@�~�@�M�@��@��@�X@��`@�Ĝ@�r�@�  @��m@��;@���@��P@�33@�
=@��H@���@�~�@�5?@�@���@��#@��-@��@�`B@�O�@�O�@�O�@�G�@�%@��D@�bN@�A�@�(�@�1@��;@���@��@�1'@�Q�@�r�@�b@��m@�1'@���@���@�\)@�+@��y@��\@��@�G�@�Ĝ@��@��@�(�@��@��;@��
@���@�1'@�I�@� �@��m@���@�\)@�o@��@��H@���@�n�@��@��#@��@�7L@��j@�bN@�Z@�Q�@�9X@�9X@�(�@��@�ƨ@���@�l�@�o@��y@�ȴ@���@�~�@�5?@��@��-@���@�`B@�?}@��@���@��`@��u@� �@�b@��
@��@��@���@���@�t�@�C�@��@�v�@�$�@��T@���@��7@�p�@�`B@�X@��@��@�Ĝ@��@�z�@��@�P@w�@pA�@hQ�@co@[@SC�@I�@D(�@>��@:n�@6ff@0�`@-/@&�y@ �9@dZ@�@S�@v�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 AƲ-AƲ-AƲ-AƲ-Aƴ9Aƴ9Aư!AƩ�AƧ�AƩ�AƩ�AƧ�AƩ�AƬA���A���AƋDA���A�5?A�bA���A���Aħ�A��A�1A�A�jA�I�A�1A���A���A��-A�Q�A�A�G�A��A��#A�;dA��RA�|�A��wA�A�A��+A��^A��PA�E�A�+A�A�A�7LA��9A��yA���A���A��A�  A��PA��A�dZA�JA��A�^5A�ȴA��uA��hA�ĜA���A���A�XA��uA���A��PA��PA��!A� �A�
=A�z�A�M�A�5?A�1A��!A�&�A��A�v�A�oA��uA�A�A�jA�I�A���A��DA���A�p�A�1'A�VA��wA��DA��jA�VA��jA��TA�bA�A�(�A�l�A���A�`BA�-A��A�I�A��A�ȴA�`BA�S�A�1A}dZA{O�Az1Ax��Av�`AuS�As��Aq��An��Am�#AlQ�Aj��Ah�\Af�!Ae"�AdZA^E�A[?}AX�!AWp�AU��ATv�ASAP(�AL5?AJ�jAH��AG|�AGVAE�
ACƨAB�AA�A@��A?S�A=ƨA=33A<�uA;|�A: �A8z�A8(�A7�^A7dZA6z�A4�`A4 �A3G�A2(�A0��A/�A-33A*��A)l�A(��A'"�A%��A%A#�TA"��A!C�A r�A�;At�A��AXA��A��A�A"�A1'AhsA�RAVA�A��AĜA�A��AK�A��A1'A��AXAbNAp�AĜA�AK�A��AA
��A
A�A	ƨA	;dA1A;dAM�A��A/A��A�!A�A�uA��AXA%A v�@�\)@��@�\)@�~�@��u@�@�1@�o@��@� �@�M�@�x�@�9@�|�@�V@��@�|�@�t�@�l�@�  @ް!@��@�?}@�bN@�C�@١�@��/@�I�@׍P@�=q@�V@ԛ�@�z�@�C�@�O�@�9X@��y@�?}@̴9@̃@�bN@�z�@��@˅@ʧ�@�-@ɉ7@ț�@�33@��@š�@�7L@ģ�@�z�@�I�@� �@�C�@�@�J@���@�X@��`@��
@���@��#@�hs@��/@��@���@���@�I�@�  @��
@��@�\)@��@��@�?}@���@�1'@���@�K�@�"�@�@��@�ȴ@�~�@���@��`@���@���@���@�A�@� �@��@�1@���@�+@�x�@�\)@�v�@�x�@�%@���@��
@�@��-@�Q�@��@�5?@���@��@��@��y@�n�@���@���@��h@�`B@�Ĝ@���@��D@��D@�bN@�1'@��F@�33@�@��@��!@�ff@�E�@�^5@�5?@�{@��^@�G�@�/@��@�Ĝ@�z�@�(�@��;@���@�33@��!@���@��\@�~�@�M�@��@��@�X@��`@�Ĝ@�r�@�  @��m@��;@���@��P@�33@�
=@��H@���@�~�@�5?@�@���@��#@��-@��@�`B@�O�@�O�@�O�@�G�@�%@��D@�bN@�A�@�(�@�1@��;@���@��@�1'@�Q�@�r�@�b@��m@�1'@���@���@�\)@�+@��y@��\@��@�G�@�Ĝ@��@��@�(�@��@��;@��
@���@�1'@�I�@� �@��m@���@�\)@�o@��@��H@���@�n�@��@��#@��@�7L@��j@�bN@�Z@�Q�@�9X@�9X@�(�@��@�ƨ@���@�l�@�o@��y@�ȴ@���@�~�@�5?@��@��-@���@�`B@�?}@��@���@��`@��u@� �@�b@��
@��@��@���@���@�t�@�C�@��@�v�@�$�@��T@���@��7@�p�@�`B@�X@��@��@�Ĝ@��@�z�@��@�P@w�@pA�@hQ�@co@[@SC�@I�@D(�@>��@:n�@6ff@0�`@-/@&�y@ �9@dZ@�@S�@v�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 ;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oBBBBBBBBB%B%B%B+B+B1B�B49Bl�B�PB�%B��B�B��B�B�B��B��B��B��B��B��B��B%B%B+B1B	7B1B
=BPBPBbBoBhBbBVBbB�B6FBQ�BW
BE�BJ�BH�BN�BF�B@�B33B6FB@�BJ�BO�BK�B+BPB  B�B�#B�B��B��B��B�{B|�BVB8RBoBJB	7B+BB��B��B�B�B�B�ZB�#B��BǮB�?B��B��B�Bk�BaHBP�B0!B�B
��B
�B
��B
�9B
��B
�7B
l�B
e`B
aHB
YB
T�B
R�B
@�B
5?B
/B
�B
	7B	��B	�B	�`B	�/B	��B	B	�}B	��B	��B	�VB	�B	u�B	iyB	k�B	K�B	8RB	{B	+B	%B��B��B�yB�BɺB�^B�FB�-B�!B�B��B��B��B��B�uB�VB�JB�=B�B�B�%B�DB�=B�1B�Bz�B{�B}�Bz�Bu�Bt�Bs�Bp�Bo�Bm�Br�Bm�Bm�Bl�BcTBgmBgmBe`BgmBk�BcTBdZBaHBbNBe`B^5B`BBcTB^5B]/B^5B]/B\)B[#B[#B[#B[#BZB[#BZBZBYBZBZBYBYB\)B^5B[#BZB[#B[#B\)B[#B[#BZBZB`BB]/BYBXBYBXBT�BYBVBS�BT�BYB`BBZBXBVBW
BVBYBR�BQ�BR�BXBR�BK�BK�BI�BI�BH�BI�BJ�BJ�BG�BI�BK�BN�BP�BT�B^5B`BBdZBgmBk�Bk�BiyBhsBhsBjBq�Bw�B~�B}�B�B�%B�JB�+B�DB�hB�hB�hB�uB��B��B��B��B��B��B��B��B��B��B��B��B��B��B�B�'B�'B�-B�'B�'B�!B�B�?B�LB�wB�}B��B��BBÖBĜBȴB��B��B��B��B��B�B�B�B�#B�;B�ZB�5B�)B�/B�;B�ZB�yB�yB�B�yB�sB�mB�sB�B�B�B�B�B�B�B�B��B��B��B��B��B	  B	B	B	B		7B	PB	VB	bB	�B	�B	�B	�B	�B	"�B	$�B	(�B	+B	,B	-B	0!B	2-B	33B	5?B	6FB	7LB	8RB	8RB	:^B	>wB	E�B	H�B	J�B	P�B	Q�B	Q�B	Q�B	Q�B	S�B	XB	YB	[#B	\)B	`BB	cTB	dZB	e`B	ffB	hsB	jB	m�B	o�B	p�B	q�B	r�B	t�B	y�B	{�B	|�B	}�B	� B	�B	�B	�+B	�7B	�JB	�uB	�{B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�!B	�9B	�LB	�XB	�dB	�wB	�}B	�}B	�}B	�}B	��B	B	ĜB	ŢB	ǮB	ȴB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�
B	�B	�B	�B	�)B	�/B	�/B	�5B	�;B	�BB	�BB	�NB	�TB	�`B	�fB	�fB	�fB	�mB	�sB	�sB	�mB	�yB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B
1B
bB
�B
�B
)�B
0!B
9XB
>wB
B�B
F�B
K�B
R�B
VB
]/B
bNB
gmB
k�B
o�B
u�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 BBBBBBBBB%B%B%B+B+B1B�B6FBo�B�\B�+B��B�B��B�#B��B��B��B��B��BB��B��B1B	7B	7B
=B
=BDBPBVBhBuB�B{BhB\B{B!�B6FBS�B^5BJ�BN�BJ�BP�BH�BC�B:^B8RBA�BM�BS�BR�B1'BoB%B�B�5B�#B�#BƨB�B��B�%B]/BA�BuBPB
=B	7BB��B��B��B�B�B�mB�BB�B��B�RB��B��B�DBn�BffBYB7LB�B1B
�B
��B
�jB
��B
�oB
o�B
gmB
dZB
\)B
XB
ZB
C�B
6FB
7LB
&�B
\B	��B	�B	�B	�NB	��B	ɺB	ǮB	��B	��B	�{B	�=B	z�B	l�B	m�B	XB	?}B	�B	
=B	
=B��B��B�B�TB��B��B�XB�9B�9B�-B��B��B��B��B��B�bB�\B�VB�=B�DB�+B�PB�JB�JB�1B}�B~�B�B� B{�B{�Bz�Bu�Br�Br�Bv�Bo�Bp�Bo�BffBiyBiyBffBiyBn�Be`Be`Be`BgmBhsBaHBbNBdZB_;B_;BaHB`BB]/B]/B]/B]/B\)B\)B_;B]/B]/B\)B\)B]/B\)B]/B^5B`BB]/B_;B^5B_;B_;B]/B\)B\)B_;BdZBaHBZBZB\)B\)BZB\)BXBXBZB]/BbNB\)B[#BYBXBW
B[#BVBS�BS�BYBXBP�BM�BJ�BJ�BJ�BK�BM�BK�BH�BK�BM�BP�BQ�BVB`BBcTBdZBiyBn�Bl�BjBiyBhsBk�Br�By�B� B� B�B�1B�VB�1B�JB�oB�oB�oB�{B��B��B��B��B��B��B��B��B��B��B��B��B��B��B�!B�-B�-B�3B�-B�-B�'B�'B�FB�LB�}B��B��BBBĜBŢB��B��B��B��B��B�B�
B�B�B�)B�BB�mB�NB�5B�;B�BB�`B�B�B�B�B�B�yB�yB�B�B�B�B�B�B�B�B��B��B��B��B��B	B	B	B	B	
=B	VB	\B	bB	�B	�B	�B	�B	 �B	"�B	$�B	)�B	,B	-B	.B	1'B	33B	49B	5?B	6FB	7LB	9XB	9XB	;dB	?}B	F�B	I�B	K�B	Q�B	Q�B	Q�B	Q�B	R�B	T�B	YB	ZB	[#B	]/B	aHB	dZB	dZB	ffB	gmB	iyB	k�B	m�B	o�B	p�B	q�B	s�B	u�B	z�B	|�B	|�B	~�B	�B	�B	�B	�+B	�7B	�JB	�{B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�!B	�9B	�RB	�^B	�jB	�}B	��B	�}B	�}B	�}B	��B	ÖB	ŢB	ƨB	ȴB	ɺB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	��B	�
B	�
B	�B	�B	�#B	�)B	�5B	�/B	�;B	�;B	�BB	�HB	�TB	�TB	�fB	�fB	�fB	�fB	�mB	�yB	�yB	�sB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B
1B
bB
�B
�B
)�B
0!B
9XB
>wB
B�B
F�B
K�B
R�B
VB
^5B
bNB
gmB
l�B
p�B
u�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 <#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
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
<#�
PRES            TEMP            PSAL            PRES_ADJUSTED = PRES - dP                                                                                                                                                                                                                                       none                                                                                                                                                                                                                                                            PSAL_ADJUSTED = sw_salt( sw_cndr(PSAL,TEMP,PRES), TEMP, PRES_ADJUSTED ) + CTM                                                                                                                                                                                   dP =0.4 dbar.                                                                                                                                                                                                                                                   none                                                                                                                                                                                                                                                            CTM: alpha=0.141, tau=6.68s                                                                                                                                                                                                                                     Pressures adjusted by using pressure offset at the sea surface. The quoted error is manufacturer specified accuracy in dbar.                                                                                                                                    The quoted error is manufacturer specified accuracy with respect to ITS-90 at time of laboratory calibration.                                                                                                                                                   No significant salinity drift detected. Salinity adjusted for effects of pressure adjustment. Conductivity cell thermal mass correction (CTM) applied to measurements taken in Cp mode. The quoted error is max[0.01, sqrt(OWerr^2+CTMerr^2)] in PSS-78.        201201101452182012011014521820120110145218  AO  ARGQ                                                                        20111130144230  QCP$                G�O�G�O�G�O�FFBFE           AO  ARGQ                                                                        20111130144230  QCF$                G�O�G�O�G�O�0               UW  ARSQUWQC    WOD & nearby Argo as visual check                               20120110145218  IP                  G�O�G�O�G�O�                