CDF      
      	DATE_TIME         STRING2       STRING4       STRING8       STRING16      STRING32       STRING64   @   	STRING256         N_PROF        N_PARAM       N_LEVELS      N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       D2011-11-30 AOML 2.2 creation; 2015-12-28T23:01:54Z UW 3.1 conversion   
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
_FillValue        G�O�   axis      Z           9�   PRES_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                    A�   PRES_ADJUSTED            
      	   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     units         decibar    	valid_min                	valid_max         F;�    
resolution        =���   C_format      %7.1f      FORTRAN_format        F7.1f      
_FillValue        G�O�        C�   PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                    K�   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     units         decibar    
resolution        =���   C_format      %7.1f      FORTRAN_format        F7.1f      
_FillValue        G�O�        M�   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     	valid_min         �      	valid_max         B      
resolution        :�o   C_format      %9.3f      FORTRAN_format        F9.3f      
_FillValue        G�O�        U�   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                    ]�   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     	valid_min         �      	valid_max         B      
resolution        :�o   C_format      %9.3f      FORTRAN_format        F9.3f      
_FillValue        G�O�        _�   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                    g�   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     units         degree_Celsius     
resolution        :�o   C_format      %9.3f      FORTRAN_format        F9.3f      
_FillValue        G�O�        i�   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    	valid_min         @      	valid_max         B$     
resolution        :�o   C_format      %9.3f      FORTRAN_format        F9.3f      
_FillValue        G�O�        q�   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                    y�   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    	valid_min         @      	valid_max         B$     
resolution        :�o   C_format      %9.3f      FORTRAN_format        F9.3f      
_FillValue        G�O�        {�   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                    ��   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     units         psu    
resolution        :�o   C_format      %9.3f      FORTRAN_format        F9.3f      
_FillValue        G�O�        ��   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  0  ��   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                    ��   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                    ��   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                    ��   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  ,  ��   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                    ��   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                    �    HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                    �   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                    �   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  @  �   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                    �L   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                    �\   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    �`   HISTORY_START_PRES                    	long_name          Start pressure action applied on   units         decibar    
_FillValue        G�O�        �p   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    units         decibar    
_FillValue        G�O�        �t   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�        �x   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    �|Argo profile    3.1 1.2 19500101000000  5901072 US ARGO PROJECT                                                 Stephen Riser                                                   PRES            TEMP            PSAL               6A   AO  20111130144041  20190522121829  1728_5048_054                   2C  D   APEX                            2142                            040306                          846 @Ա���1   @Ա����@5�KƧ��c�C��1   GPS     Primary sampling: mixed [deeper than nominal 990dbar: discrete; nominal 990dbar to surface: 2dbar-bin averaged]                                                                                                                                                    A   A   A   @���@�  A   A   A@  A`  A~ffA�  A�  A�  A�  A�  A�  A�  B   B  B  B  B   B(  B0  B8  B@  BH  BP  BX  B`  Bh  Bp  Bx  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�ffB���B���B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  C   C  C  C  C  C
  C  C  C  C  C  C  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CQ�fCT  CV�CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D �fD  D� D  Dy�D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D��D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D��D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&�fD'  D'� D(  D(� D(��D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.y�D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DCfDC�fDD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dcy�Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dt� Du  Du� Dv  DvffDyY�D�  D�<�D���D��fD��fD�#3D���D���D���D�)�D�VfD�ɚD��fD�fDڃ3D���D�� D�3D�,�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111@�  @�33@�33A��A9��AY��Ax  A���A���A���A���A���A���A���A���BffBffBffBffB&ffB.ffB6ffB>ffBFffBNffBVffB^ffBfffBnffBvffB~ffB�33B�33B�33B�33B�33B�33B�33B�33B�33B�33B�33B�33B���B�  B�  B�33B�33B�33B�33B�33B�33B�33B�33B�33B�33B�33B�33B�33B�33B�33B�33B�33C��C��C��C��C	��C��C��C��C��C��C��C��C��C��C��C��C!��C#��C%��C'��C)��C+��C-��C/��C1��C3��C5��C7��C9��C;��C=��C?��CA��CC��CE��CG��CI��CK��CM��CO��CQ� CS��CU�3CW��CY��C[��C]��C_��Ca��Cc��Ce��Cg��Ci��Ck��Cm��Co��Cq��Cs��Cu��Cw��Cy��C{��C}��C��C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C�ٚC���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���D l�D �fDffD�fD` D�fDffD�fDffD�fDffD�fDffD�fDffD�fDffD�fD	ffD	�fD
ffD
�fDffD� DffD�fDffD�fDffD�fDffD�fDffD�fDffD�fDffD�fDffD�fDffD�fDffD�fDffD�fDffD�fDffD�fDffD�fDffD�fDffD�fDffD� DffD�fDffD�fDffD�fD ffD �fD!ffD!�fD"ffD"�fD#ffD#�fD$ffD$�fD%ffD%�fD&l�D&�fD'ffD'�fD(ffD(� D)ffD)�fD*ffD*�fD+ffD+�fD,ffD,�fD-ffD-�fD.` D.�fD/ffD/�fD0ffD0�fD1ffD1�fD2ffD2�fD3ffD3�fD4ffD4�fD5ffD5�fD6ffD6�fD7ffD7�fD8ffD8�fD9ffD9�fD:ffD:�fD;ffD;�fD<ffD<�fD=ffD=�fD>ffD>�fD?ffD?�fD@ffD@�fDAffDA�fDBffDB��DCl�DC�fDDffDD�fDEffDE�fDFffDF�fDGffDG�fDHffDH�fDIffDI�fDJffDJ�fDKffDK�fDLffDL�fDMffDM�fDNffDN�fDOffDO�fDPffDP�fDQffDQ�fDRffDR�fDSffDS�fDTffDT�fDUffDU�fDVffDV�fDWffDW�fDXffDX�fDYffDY�fDZffDZ�fD[ffD[�fD\ffD\�fD]ffD]�fD^ffD^�fD_ffD_�fD`ffD`�fDaffDa�fDbffDb�fDc` Dc�fDdffDd�fDeffDe�fDfffDf�fDgffDg�fDhffDh�fDiffDi�fDjffDj�fDkffDk�fDlffDl�fDmffDm�fDnffDn�fDoffDo�fDpffDp�fDqffDq�fDrffDr�fDsffDs�fDtffDt�fDuffDu�fDvL�Dy@ D��3D�0 D�|�D���D��D�fD�|�D�� D�� D��D�I�DǼ�D��D�	�D�vfD�� D��3D��fD�  11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��A��`A��TA��HA��TA��TA��TA��TA���A®A�A�A�A�A�AhAPA7AA�|�A�v�A�p�A�jA�dZA�`BA�XA�VA�S�A�Q�A�Q�A�VA�VA�Q�A�I�A�C�A�9XA�9XA�33A�(�A�&�A�JA��A��wA���A��wA�x�A��;A�`BA��A��A���A���A��wA��PA��A��A�A��uA�1'A���A�=qA���A�G�A�A���A�&�A�=qA��A�;dA��A���A�ZA��A���A�S�A��^A���A�^5A��RA�bA�E�A�G�A��jA�I�A�l�A��jA���A�~�A�ƨA�-A�XA��!A�\)A��-A��jA���A���A�7LA��A�C�A�O�A�G�A��DA�t�A��hA�|�A���A���A�ȴA�1'A�XA�5?A�dZA��A��A}\)A{�TAy��Aw�Au\)As�
Aq��Ao��An  AiXAf��Ac�wA`��A^��A^�A^�A\Q�AZ-AX1'AWhsAT9XAR�HAQ��AN�AL-AJ�\AJ9XAI��AH�yAG��AGG�AF�9AEp�AC��AA��A>I�A;�A9�wA8(�A7&�A6=qA4��A3VA2A�A1&�A0n�A/�FA/A.�A,��A,jA+�^A*��A)?}A(I�A'A'"�A&Q�A$�!A$bNA#�A"$�A!�A bNA�A{At�Az�A�PA�!A�
A{A;dA=qA��AĜA��A/Ar�A�A��A1A��A�A��A+AȴAJAO�A
�yA��AA�An�AƨA��AbNA��Av�A?}A ��A ��A n�A J@���@��@��y@��7@���@�9X@��T@�@���@��@�$�@웦@�t�@陚@�D@�
=@�h@��@���@�I�@�33@�n�@�X@��@�I�@�$�@�1'@��y@�7L@׾w@��H@�`B@�+@��@ύP@͙�@�Ĝ@�|�@ʰ!@��/@�\)@��@Å@�n�@�?}@�  @�+@���@�=q@�J@�O�@� �@��;@���@�
=@�5?@�%@�j@���@�K�@�M�@���@�V@�Ĝ@�9X@���@�+@��R@�^5@�$�@���@�G�@�b@���@��@��R@��7@�I�@���@�33@���@���@��+@��T@�=q@�|�@�l�@��!@���@�@�?}@���@� �@�t�@���@��R@�5?@���@�hs@��@���@���@�I�@�1@��
@��@�t�@�;d@���@�5?@�J@��@���@�  @��
@�ƨ@���@�"�@��@�M�@�J@��^@��@�p�@�%@��D@�1'@�  @��w@��@��w@���@�dZ@�K�@�C�@�o@���@��@��-@�`B@�O�@�G�@�?}@�&�@��@�%@��/@�Ĝ@��j@���@��D@�r�@�1'@��@���@��F@���@�t�@�\)@�33@�o@��@���@�V@��^@�G�@�&�@��@���@�r�@�A�@��@���@���@�z�@�z�@�bN@�9X@�C�@�ȴ@���@���@��+@�n�@�ff@�^5@�M�@�5?@�-@�$�@���@��7@�X@�&�@���@��9@�Q�@�1'@��;@�|�@�@�ȴ@���@�ff@�5?@���@�@���@���@��@��^@��7@�G�@�V@��@��`@��`@���@�r�@��@��@��m@��;@��;@��
@��@�K�@��@��y@��R@���@���@��\@�ff@�M�@�$�@���@�G�@�&�@�7L@�x�@�p�@�7L@���@��@�j@�9X@�(�@��@��
@�S�@�33@��@��@���@�M�@�5?@��@��#@��7@�hs@�7L@��/@���@��u@�r�@�bN@���@���@�l�@�"�@���@��H@��y@���@zM�@n�y@h�@a��@Y7L@PĜ@IG�@D�@?�;@97L@333@-�T@&�y@"-@@��@�j@��@��11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111A��`A��TA��HA��TA��TA��TA��TA���A®A�A�A�A�A�AhAPA7AA�|�A�v�A�p�A�jA�dZA�`BA�XA�VA�S�A�Q�A�Q�A�VA�VA�Q�A�I�A�C�A�9XA�9XA�33A�(�A�&�A�JA��A��wA���A��wA�x�A��;A�`BA��A��A���A���A��wA��PA��A��A�A��uA�1'A���A�=qA���A�G�A�A���A�&�A�=qA��A�;dA��A���A�ZA��A���A�S�A��^A���A�^5A��RA�bA�E�A�G�A��jA�I�A�l�A��jA���A�~�A�ƨA�-A�XA��!A�\)A��-A��jA���A���A�7LA��A�C�A�O�A�G�A��DA�t�A��hA�|�A���A���A�ȴA�1'A�XA�5?A�dZA��A��A}\)A{�TAy��Aw�Au\)As�
Aq��Ao��An  AiXAf��Ac�wA`��A^��A^�A^�A\Q�AZ-AX1'AWhsAT9XAR�HAQ��AN�AL-AJ�\AJ9XAI��AH�yAG��AGG�AF�9AEp�AC��AA��A>I�A;�A9�wA8(�A7&�A6=qA4��A3VA2A�A1&�A0n�A/�FA/A.�A,��A,jA+�^A*��A)?}A(I�A'A'"�A&Q�A$�!A$bNA#�A"$�A!�A bNA�A{At�Az�A�PA�!A�
A{A;dA=qA��AĜA��A/Ar�A�A��A1A��A�A��A+AȴAJAO�A
�yA��AA�An�AƨA��AbNA��Av�A?}A ��A ��A n�A J@���@��@��y@��7@���@�9X@��T@�@���@��@�$�@웦@�t�@陚@�D@�
=@�h@��@���@�I�@�33@�n�@�X@��@�I�@�$�@�1'@��y@�7L@׾w@��H@�`B@�+@��@ύP@͙�@�Ĝ@�|�@ʰ!@��/@�\)@��@Å@�n�@�?}@�  @�+@���@�=q@�J@�O�@� �@��;@���@�
=@�5?@�%@�j@���@�K�@�M�@���@�V@�Ĝ@�9X@���@�+@��R@�^5@�$�@���@�G�@�b@���@��@��R@��7@�I�@���@�33@���@���@��+@��T@�=q@�|�@�l�@��!@���@�@�?}@���@� �@�t�@���@��R@�5?@���@�hs@��@���@���@�I�@�1@��
@��@�t�@�;d@���@�5?@�J@��@���@�  @��
@�ƨ@���@�"�@��@�M�@�J@��^@��@�p�@�%@��D@�1'@�  @��w@��@��w@���@�dZ@�K�@�C�@�o@���@��@��-@�`B@�O�@�G�@�?}@�&�@��@�%@��/@�Ĝ@��j@���@��D@�r�@�1'@��@���@��F@���@�t�@�\)@�33@�o@��@���@�V@��^@�G�@�&�@��@���@�r�@�A�@��@���@���@�z�@�z�@�bN@�9X@�C�@�ȴ@���@���@��+@�n�@�ff@�^5@�M�@�5?@�-@�$�@���@��7@�X@�&�@���@��9@�Q�@�1'@��;@�|�@�@�ȴ@���@�ff@�5?@���@�@���@���@��@��^@��7@�G�@�V@��@��`@��`@���@�r�@��@��@��m@��;@��;@��
@��@�K�@��@��y@��R@���@���@��\@�ff@�M�@�$�@���@�G�@�&�@�7L@�x�@�p�@�7L@���@��@�j@�9X@�(�@��@��
@�S�@�33@��@��@���@�M�@�5?@��@��#@��7@�hs@�7L@��/@���@��u@�r�@�bN@���@���@�l�@�"�@���@��H@��y@���@zM�@n�y@h�@a��@Y7L@PĜ@IG�@D�@?�;@97L@333@-�T@&�y@"-@@��@�j@��@��11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oB��B��B�B�B�B��B��B��B��B��B��B��B��B��B��B��B�B�B�B�B�B��B��B��B��B��B��B��B�B�B�B�B�B�B�!B�-B�9B�FB�LBĜB��B�B�BB�BȴBbB9XBN�BdZBgmBp�Bx�B�JB��B��B��B��B��B��B�B�B��B��B��B��B��B�uB��B�VB�hB�bB�7B�VB�Bw�Bt�Bv�Bw�B�B]/B[#B]/BP�BL�B?}B;dB7LB/B&�B�BhB+B��B�B�yB�;B�RB��B�VBm�BQ�B:^B#�B
��B
�NB
��B
�3B
�B
m�B
cTB
YB
Q�B
>wB
?}B
�B

=B	��B	�B	�fB	�BB	�/B	�
B	��B	x�B	k�B	N�B	O�B	J�B	N�B	D�B	5?B	&�B	�B	JB	  B��B�sB�)B�
B��B��B��B��B��B��BɺBB�dB�XB�^B��B��B��B��B�oB�{B��B�{B��B�\B�uB�oB�hB�%B�7B~�Bz�Bx�Bu�Bs�Bl�BhsBe`BffBdZBaHBbNBdZB\)B^5B_;B[#BW
BP�BP�BP�BQ�BQ�BQ�BO�BO�BR�BQ�BQ�BO�BO�BR�BO�BO�BN�BT�BO�BO�BQ�BM�BE�BI�BO�BP�B?}B=qBB�BH�B=qB=qB=qB=qB?}BJ�BB�B>wB?}B>wBVBG�B>wB6FB6FB9XB49B33B2-B33B1'B0!B/B1'B/B.B/B-B+B)�B(�B&�B'�B)�B+B$�B&�B&�B,B/B0!B/B-B+B,B,B.B-B/B1'B49B5?B5?B5?B;dBC�BG�BI�BL�BN�BR�BVBW
BXB\)B_;BbNBcTBgmBiyBm�Bq�Bt�Bu�Bx�B~�B�7B�uB��B��B��B��B��B��B��B��B��B��B��B�?B�XB�^B�wB��BĜBŢBǮB��B��B��B��B�#B�BB�`B�yB�B�B��B��B��B��B	  B	B	B	+B	1B	JB	JB	\B	oB	uB	�B	�B	�B	�B	"�B	)�B	2-B	49B	49B	5?B	6FB	8RB	<jB	A�B	B�B	B�B	C�B	C�B	F�B	F�B	F�B	H�B	L�B	M�B	O�B	Q�B	S�B	S�B	VB	YB	\)B	_;B	aHB	ffB	iyB	k�B	l�B	m�B	n�B	p�B	r�B	t�B	x�B	y�B	z�B	}�B	� B	�B	�B	�B	�%B	�+B	�7B	�PB	�uB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�B	�-B	�3B	�9B	�9B	�3B	�9B	�FB	�^B	�^B	�jB	�wB	�wB	��B	��B	��B	��B	B	ÖB	ŢB	ƨB	ǮB	ɺB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�
B	�B	�B	�B	�B	�B	�B	�B	�#B	�/B	�;B	�BB	�HB	�TB	�TB	�ZB	�ZB	�ZB	�fB	�sB	�sB	�sB	�sB	�yB	�yB	�sB	�yB	�sB	�sB	�yB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B

=B
oB
�B
!�B
+B
2-B
8RB
?}B
C�B
H�B
M�B
S�B
[#B
`BB
gmB
jB
n�B
r�B
u�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111B��B��B�B�B�B��B�B��B��B��B��B��B��B��B��B��B�B�B�B�B�B��B��B��B��B��B��B��B�B�B�B�B�B�B�!B�-B�9B�FB�LBĜB��B�B��B1B��B��BhB:^BR�BgmBhsBq�B|�B�hB��B��B��B��B��B�B�B�B��B�B��B��B��B��B��B�bB�{B�uB�DB�hB�1By�Bw�By�B{�B�7B`BB^5BbNBT�BP�BB�B?}B;dB49B+B�B�BJB  B�B�B�B�qB�B��Bv�BYB@�B0!B%B
�sB
��B
��B
�\B
n�B
gmB
]/B
S�B
D�B
C�B
 �B
hB
B	�B	�B	�`B	�TB	�TB	��B	�B	s�B	S�B	Q�B	L�B	S�B	J�B	;dB	)�B	%�B	bB	B	B�B�HB�B�
B��B��B��B��B��B��BɺBŢB�}B�}B�B��B��B��B��B��B��B��B��B�hB��B��B�uB�1B�DB�B}�Bz�Bw�Bv�Bq�BiyBiyBiyBgmBcTBffBgmB^5BaHBbNB^5BZBVBS�BS�BS�BT�BVBQ�BR�BVBT�BT�BQ�BR�BW
BQ�BQ�BQ�BW
BP�BT�BT�BO�BG�BK�BR�BQ�BA�B@�BE�BJ�B>wB>wB>wB?}BC�BL�BD�B?}B@�BB�BXBL�BA�B8RB9XB;dB7LB5?B5?B6FB2-B1'B0!B33B1'B0!B0!B.B/B-B+B)�B+B,B.B(�B)�B+B0!B1'B33B/B0!B.B/B,B0!B/B1'B33B5?B6FB6FB7LB=qBD�BH�BJ�BN�BP�BS�BW
BYBZB\)B`BBcTBdZBhsBjBn�Br�Bu�Bv�By�B�B�=B�{B��B��B��B��B��B��B��B��B��B��B��B�?B�^B�dB�}BBŢBƨBȴB��B��B��B��B�)B�HB�fB�B�B�B��B��B��B��B	B	B	%B	1B	
=B	PB	JB	\B	uB	{B	�B	�B	�B	�B	#�B	)�B	33B	5?B	5?B	6FB	7LB	8RB	<jB	B�B	C�B	B�B	C�B	D�B	G�B	G�B	G�B	I�B	L�B	M�B	O�B	Q�B	S�B	S�B	W
B	YB	\)B	_;B	aHB	ffB	jB	l�B	l�B	m�B	o�B	q�B	r�B	u�B	y�B	z�B	{�B	~�B	�B	�B	�B	�%B	�+B	�1B	�=B	�PB	�uB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�!B	�3B	�9B	�?B	�9B	�9B	�?B	�LB	�dB	�dB	�qB	�}B	�}B	��B	��B	��B	��B	ÖB	ĜB	ƨB	ǮB	ǮB	ɺB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	��B	��B	�
B	�B	�B	�#B	�B	�#B	�B	�B	�#B	�/B	�BB	�HB	�NB	�ZB	�ZB	�ZB	�ZB	�`B	�mB	�yB	�sB	�yB	�yB	�B	�yB	�sB	�B	�yB	�yB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B

=B
oB
�B
!�B
,B
33B
8RB
@�B
C�B
H�B
M�B
S�B
[#B
`BB
gmB
jB
n�B
r�B
u�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
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
<49X<#�
<#�
<#�
<49X<#�
<#�
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
<#�
<#�
PRES            TEMP            PSAL            PRES_ADJUSTED = PRES - dP                                                                                                                                                                                                                                       none                                                                                                                                                                                                                                                            PSAL_ADJUSTED = sw_salt( sw_cndr(PSAL,TEMP,PRES), TEMP, PRES_ADJUSTED ) + CTM                                                                                                                                                                                   dP =0.4 dbar.                                                                                                                                                                                                                                                   none                                                                                                                                                                                                                                                            CTM: alpha=0.141, tau=6.68s                                                                                                                                                                                                                                     Pressures adjusted by using pressure offset at the sea surface. The quoted error is manufacturer specified accuracy in dbar.                                                                                                                                    The quoted error is manufacturer specified accuracy with respect to ITS-90 at time of laboratory calibration.                                                                                                                                                   No significant salinity drift detected. Salinity adjusted for effects of pressure adjustment. Conductivity cell thermal mass correction (CTM) applied to measurements taken in Cp mode. The quoted error is max[0.01, sqrt(OWerr^2+CTMerr^2)] in PSS-78.        201201101452122012011014521220120110145212  AO  ARGQ                                                                        20111130144041  QCP$                G�O�G�O�G�O�FFBFE           AO  ARGQ                                                                        20111130144041  QCF$                G�O�G�O�G�O�0               UW  ARSQUWQC    WOD & nearby Argo as visual check                               20120110145212  IP                  G�O�G�O�G�O�                