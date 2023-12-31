CDF      
      	DATE_TIME         STRING2       STRING4       STRING8       STRING16      STRING32       STRING64   @   	STRING256         N_PROF        N_PARAM       N_LEVELS     N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       D2011-11-30 AOML 2.2 creation; 2015-12-28T23:01:59Z UW 3.1 conversion   
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
_FillValue        G�O�   axis      Z          9�   PRES_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                   A�   PRES_ADJUSTED            
      	   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     units         decibar    	valid_min                	valid_max         F;�    
resolution        =���   C_format      %7.1f      FORTRAN_format        F7.1f      
_FillValue        G�O�       C�   PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                   K�   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     units         decibar    
resolution        =���   C_format      %7.1f      FORTRAN_format        F7.1f      
_FillValue        G�O�       M�   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     	valid_min         �      	valid_max         B      
resolution        :�o   C_format      %9.3f      FORTRAN_format        F9.3f      
_FillValue        G�O�       U�   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                   ]�   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     	valid_min         �      	valid_max         B      
resolution        :�o   C_format      %9.3f      FORTRAN_format        F9.3f      
_FillValue        G�O�       _�   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                   g�   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     units         degree_Celsius     
resolution        :�o   C_format      %9.3f      FORTRAN_format        F9.3f      
_FillValue        G�O�       i�   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    	valid_min         @      	valid_max         B$     
resolution        :�o   C_format      %9.3f      FORTRAN_format        F9.3f      
_FillValue        G�O�       q�   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                   y�   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    	valid_min         @      	valid_max         B$     
resolution        :�o   C_format      %9.3f      FORTRAN_format        F9.3f      
_FillValue        G�O�       {�   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                   ��   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     units         psu    
resolution        :�o   C_format      %9.3f      FORTRAN_format        F9.3f      
_FillValue        G�O�       ��   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  0  �    SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                    �0   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                    �0   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                    �0   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  ,  �0   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                    �\   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                    �`   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                    �d   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                    �h   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  @  �l   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                    ��   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                    ��   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    ��   HISTORY_START_PRES                    	long_name          Start pressure action applied on   units         decibar    
_FillValue        G�O�        ��   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    units         decibar    
_FillValue        G�O�        ��   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�        ��   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    ��Argo profile    3.1 1.2 19500101000000  5901072 US ARGO PROJECT                                                 Stephen Riser                                                   PRES            TEMP            PSAL               MA   AO  20111130144309  20190522121829  1728_5048_077                   2C  D   APEX                            2142                            040306                          846 @���`��1   @��)���@6U?|�h�b�p��
=1   GPS     Primary sampling: mixed [deeper than nominal 990dbar: discrete; nominal 990dbar to surface: 2dbar-bin averaged]                                                                                                                                                    A   A   A   @�ff@�  A   A   A@  A`  A�  A�  A�33A�  A�  A�  A�  A�  B   B  B  B  B   B'��B0  B8  B@  BH  BP  BX  B`  Bh  Bp  Bx  B�  B�  B�  B�  B�  B�  B�  B�33B�33B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  C   C  C  C  C  C
  C  C  C  C  C  C  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>�C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D�fD  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'�fD(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D=��D>y�D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE�fDF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  Dey�Df  Df�fDg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� DofDo� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dt� Du  Du� Dv  Dv� Dw  Dw� Dy��D���D�)�D�p D��fD�	�D�@ D�� D���D��fD��D��fDǳ3D���D�  Dڃ3D�fD��3D�3D�@ 1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  @s33@�33@�33A��A9��AY��Ay��A���A�  A���A���A���A���A���A���BffBffBffBffB&  B.ffB6ffB>ffBFffBNffBVffB^ffBfffBnffBvffB~ffB�33B�33B�33B�33B�33B�33B�ffB�ffB�33B�33B�33B�33B�33B�33B�33B�33B�33B�33B�33B�33B�33B�33B�33B�33B�33B�33B�33B�33B�33B�33B�33B�33C��C��C��C��C	��C��C��C��C��C��C��C��C��C��C��C��C!��C#��C%��C'��C)��C+��C-��C/��C1��C3��C5��C7��C9��C;��C=�3C?��CA��CC��CE��CG��CI��CK��CM��CO��CQ��CS��CU��CW��CY��C[��C]��C_��Ca��Cc��Ce��Cg��Ci��Ck��Cm��Co��Cq��Cs��Cu��Cw��Cy��C{��C}��C��C���C���C���C���C���C���C���C���C���C���C���C���C���C�ٚC���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C�� C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C�� D ffD �fDffD�fDffD�fDffD�fDffD�fDffD�fDffD�fDffD�fDffD�fD	ffD	�fD
ffD
�fDffD�fDffD�fDffD�fDffD�fDffD�fDffD�fDffD�fDffD�fDffD�fDffD�fDffD�fDffD�fDffD�fDffD�fDffD�fDffD�fDl�D�fDffD�fDffD�fDffD�fDffD�fD ffD �fD!ffD!�fD"ffD"�fD#ffD#�fD$ffD$�fD%ffD%�fD&ffD&�fD'l�D'�fD(ffD(�fD)ffD)�fD*ffD*�fD+ffD+�fD,ffD,�fD-ffD-�fD.ffD.�fD/ffD/�fD0ffD0�fD1ffD1�fD2ffD2�fD3ffD3�fD4ffD4�fD5ffD5�fD6ffD6�fD7ffD7�fD8ffD8�fD9ffD9�fD:ffD:�fD;ffD;�fD<ffD<�fD=ffD=� D>` D>�fD?ffD?�fD@ffD@�fDAffDA�fDBffDB�fDCffDC�fDDffDD�fDEl�DE�fDFffDF�fDGffDG�fDHffDH�fDIffDI�fDJffDJ�fDKffDK�fDLffDL�fDMffDM�fDNffDN�fDOffDO�fDPffDP�fDQffDQ�fDRffDR�fDSffDS�fDTffDT�fDUffDU�fDVffDV�fDWffDW�fDXffDX�fDYffDY�fDZffDZ�fD[ffD[�fD\ffD\�fD]ffD]�fD^ffD^�fD_ffD_�fD`ffD`�fDaffDa�fDbffDb�fDcffDc�fDdffDd�fDe` De�fDfl�Df�fDgffDg�fDhffDh�fDiffDi�fDjffDj�fDkffDk�fDlffDl�fDmffDm�fDnffDn��DoffDo�fDpffDp�fDqffDq�fDrffDr�fDsffDs�fDtffDt�fDuffDu�fDvffDv�fDwffDy�3D���D��D�c3D���D���D�33D�s3D�� D�ɚD��D�y�DǦfD�� D�3D�vfD���D��fD�fD�331111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  @��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��A���A���A��A��A��yA��/AʅA�33AɅA�;dA� �A� �A�(�A�33A�Q�A�p�A�VA��A���A��
A�jA�Q�A�I�A�=qA�+A��A�  A��A��A��A��yA��TA��A�ȴAǺ^Aǥ�A�~�A�1A�K�A���A��A��DA�dZA���A�hsA�ĜA�/A�?}A�9XA�  A���A��FA��A�\)A��#A��+A�$�A�|�A�n�A�A�\)A��7A��HA�z�A���A�x�A�\)A��+A���A��+A�7LA���A�\)A�l�A��A��+A�hsA�  A���A�G�A�{A�A��;A��FA�E�A� �A��;A��A�A�A���A�G�A�r�A�bNA��A��A�C�A��RA��FA��TA�5?A��A�$�A��uA��A�%A�A���A�t�A���A��A��\A��RA��;A�;dA��uAA~��A}�A}oA{K�Axr�Au��Arr�Apv�Am
=Aj�Ae��Aa`BA_33A]�
A[XAZJAX��AT�AQ��AP5?AO�FAO;dAN�AN�\AM��AL��AK�AJM�AG�-AF�AF(�AD�+AB��ABbA@{A=�PA<~�A<(�A;��A;�^A;&�A9ƨA7
=A5K�A4�A2��A1�A0A.�\A-7LA*�A*^5A*{A)A(�HA(ffA'+A&ZA%ƨA%�FA%A%t�A%"�A$��A#S�A"A�A!O�A ��A r�A�AXAffA�;AM�AVAr�A�AJA;dAȴAE�A+A��A��AĜA�TA�AĜAz�A�A�RA�A�`A��A=qA
��A	t�A	�A�DAdZA�jAI�A�;A�yAdZAVA A�@��R@���@��@�G�@�l�@��/@�\@�7@��@�@�v�@���@���@�D@�bN@�"�@�7@�1@��y@�{@�x�@�V@�Ĝ@�Q�@��;@�@���@�o@��@���@�ff@�b@��@���@ԛ�@Ӯ@ҧ�@�p�@ϝ�@���@��/@�Z@�dZ@�V@��/@��
@��@î@�$�@�`B@���@�I�@�t�@�E�@�b@�-@��@��@���@��#@���@�;d@��\@�@�V@���@��@�Z@���@�C�@���@�~�@�{@�p�@�1@�|�@�33@��H@���@�~�@�-@�?}@���@��@�bN@�b@��@�"�@��+@�$�@��T@�O�@��@�9X@���@��F@�;d@�C�@�
=@��H@�dZ@��D@���@��m@�"�@��H@��+@��@�x�@��@��w@�-@�~�@���@��+@�{@�x�@��@�1'@��P@�
=@��H@��!@���@��!@�ȴ@���@���@�M�@���@��h@�$�@�~�@�~�@�ff@��!@�\)@�+@�n�@���@��@�z�@�p�@�p�@�%@�K�@���@��u@�9X@�Q�@�r�@�r�@�z�@�bN@��@��;@��w@���@�\)@�@�ȴ@���@�~�@�E�@�-@��@���@�p�@�O�@�/@�&�@�V@���@��@��/@�Ĝ@�j@��m@���@�t�@�C�@�o@�ȴ@���@��\@�5?@��@���@�X@�?}@�&�@�%@��@���@�r�@�Q�@�A�@�1'@�b@���@�ƨ@���@�t�@�K�@�
=@���@��+@�ff@�=q@���@�X@�O�@�G�@�7L@�&�@��@�%@��@��9@�(�@��F@�S�@�K�@�;d@��@��@��H@��R@���@��\@�n�@�M�@�-@��@���@�/@��/@�Ĝ@��9@��@���@�z�@�bN@�Q�@�A�@�9X@�(�@�b@�  @��;@��@��P@�\)@�"�@�@��@���@���@�v�@�V@�J@���@���@�hs@��@���@�Ĝ@��9@��@�Z@�1'@�C�@K�@w�w@o�;@gl�@^ȴ@S�F@Lz�@EO�@@  @9hs@3S�@-�@'�P@$(�@;d@33@\)@9X@�`1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  A���A���A��A��A��yA��/AʅA�33AɅA�;dA� �A� �A�(�A�33A�Q�A�p�A�VA��A���A��
A�jA�Q�A�I�A�=qA�+A��A�  A��A��A��A��yA��TA��A�ȴAǺ^Aǥ�A�~�A�1A�K�A���A��A��DA�dZA���A�hsA�ĜA�/A�?}A�9XA�  A���A��FA��A�\)A��#A��+A�$�A�|�A�n�A�A�\)A��7A��HA�z�A���A�x�A�\)A��+A���A��+A�7LA���A�\)A�l�A��A��+A�hsA�  A���A�G�A�{A�A��;A��FA�E�A� �A��;A��A�A�A���A�G�A�r�A�bNA��A��A�C�A��RA��FA��TA�5?A��A�$�A��uA��A�%A�A���A�t�A���A��A��\A��RA��;A�;dA��uAA~��A}�A}oA{K�Axr�Au��Arr�Apv�Am
=Aj�Ae��Aa`BA_33A]�
A[XAZJAX��AT�AQ��AP5?AO�FAO;dAN�AN�\AM��AL��AK�AJM�AG�-AF�AF(�AD�+AB��ABbA@{A=�PA<~�A<(�A;��A;�^A;&�A9ƨA7
=A5K�A4�A2��A1�A0A.�\A-7LA*�A*^5A*{A)A(�HA(ffA'+A&ZA%ƨA%�FA%A%t�A%"�A$��A#S�A"A�A!O�A ��A r�A�AXAffA�;AM�AVAr�A�AJA;dAȴAE�A+A��A��AĜA�TA�AĜAz�A�A�RA�A�`A��A=qA
��A	t�A	�A�DAdZA�jAI�A�;A�yAdZAVA A�@��R@���@��@�G�@�l�@��/@�\@�7@��@�@�v�@���@���@�D@�bN@�"�@�7@�1@��y@�{@�x�@�V@�Ĝ@�Q�@��;@�@���@�o@��@���@�ff@�b@��@���@ԛ�@Ӯ@ҧ�@�p�@ϝ�@���@��/@�Z@�dZ@�V@��/@��
@��@î@�$�@�`B@���@�I�@�t�@�E�@�b@�-@��@��@���@��#@���@�;d@��\@�@�V@���@��@�Z@���@�C�@���@�~�@�{@�p�@�1@�|�@�33@��H@���@�~�@�-@�?}@���@��@�bN@�b@��@�"�@��+@�$�@��T@�O�@��@�9X@���@��F@�;d@�C�@�
=@��H@�dZ@��D@���@��m@�"�@��H@��+@��@�x�@��@��w@�-@�~�@���@��+@�{@�x�@��@�1'@��P@�
=@��H@��!@���@��!@�ȴ@���@���@�M�@���@��h@�$�@�~�@�~�@�ff@��!@�\)@�+@�n�@���@��@�z�@�p�@�p�@�%@�K�@���@��u@�9X@�Q�@�r�@�r�@�z�@�bN@��@��;@��w@���@�\)@�@�ȴ@���@�~�@�E�@�-@��@���@�p�@�O�@�/@�&�@�V@���@��@��/@�Ĝ@�j@��m@���@�t�@�C�@�o@�ȴ@���@��\@�5?@��@���@�X@�?}@�&�@�%@��@���@�r�@�Q�@�A�@�1'@�b@���@�ƨ@���@�t�@�K�@�
=@���@��+@�ff@�=q@���@�X@�O�@�G�@�7L@�&�@��@�%@��@��9@�(�@��F@�S�@�K�@�;d@��@��@��H@��R@���@��\@�n�@�M�@�-@��@���@�/@��/@�Ĝ@��9@��@���@�z�@�bN@�Q�@�A�@�9X@�(�@�b@�  @��;@��@��P@�\)@�"�@�@��@���@���@�v�@�V@�J@���@���@�hs@��@���@�Ĝ@��9@��@�Z@�1'@�C�@K�@w�w@o�;@gl�@^ȴ@S�F@Lz�@EO�@@  @9hs@3S�@-�@'�P@$(�@;d@33@\)@9X@�`1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  ;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oBVBVB\BVBhBhB�B)�B�bB�?BBĜBǮB��B�ZB��BhB�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�BhB%B�ZB��BŢB��BɺBƨBŢBȴB�NB  BBBBB	7BVBbBhBVB\BbBoBoBoBoB�B\B	7B%B\BVBVB
=B+B��B�B�B�B�fB�#B��B��B��BƨB��B�9B��B�DBffBVBN�B@�B,BhB	7B�B�
B�dB�-B��B�uB�%Bn�BcTBW
BE�B1'B�BB
�5B
��B
��B
�\B
� B
]/B
D�B
9XB
0!B
,B
�B
hB	��B	�B	�jB	�!B	�uB	x�B	ZB	C�B	33B	'�B	�B	�B	PB��B�B�TB�NB�;B�5B�#B�#B�B�/B�B��B��B��BɺBÖB��B�^B�3B�B�B�9BŢB��BȴB�^B�'B�3B�!B�B��B��B��B��B��B��B��B��B�VB�PB�+B�B�+B�PB�hB�hB�bB�%B�B|�Bv�Bu�Bv�Br�Bw�Bs�Bs�Bk�BjBk�B`BBaHB_;B]/B[#BW
BYBXBW
BT�BS�BR�BVBQ�BL�BK�BO�BP�BT�BS�BQ�BN�BK�BK�BJ�BJ�BG�BJ�B>wBA�BC�B:^B5?B=qB1'B.B2-B/B49B-B,B,B)�B+B-B/B+B-B-B.B.B.B.B-B,B)�B)�B&�B&�B(�B(�B'�B(�B+B-B-B-B.B/B/B.B/B0!B/B0!B.B2-B5?B8RB9XB:^B:^B<jBA�B@�BF�BE�BG�BXBM�BZBYBZBbNBe`Be`BffBjBn�Bo�Bs�Bw�Bz�B|�B�+B�%B�1B�7B�=B�DB�PB�oB��B��B��B��B��B��B��B��B��B��B�B�3B�?B�RB�}BÖBĜBǮB��B�#B�BB�HB�ZB�sB�sB�B�B��B	B��B��B		7B	
=B	JB	VB	hB	uB	{B	�B	�B	�B	�B	�B	�B	!�B	%�B	)�B	'�B	(�B	0!B	6FB	7LB	;dB	>wB	G�B	G�B	E�B	D�B	E�B	G�B	P�B	S�B	Q�B	L�B	O�B	_;B	bNB	e`B	iyB	l�B	q�B	s�B	w�B	z�B	|�B	~�B	�B	�B	�+B	�1B	�7B	�7B	�PB	�bB	�{B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�B	�3B	�-B	�'B	�'B	�-B	�3B	�FB	�FB	�LB	�LB	�RB	�^B	�jB	�wB	�qB	�wB	�}B	��B	B	B	B	ÖB	ǮB	ȴB	ȴB	ȴB	ȴB	ɺB	ɺB	ɺB	ɺB	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�
B	�
B	�
B	�B	�B	�B	�#B	�)B	�5B	�;B	�;B	�;B	�;B	�BB	�BB	�BB	�HB	�HB	�HB	�HB	�NB	�NB	�TB	�TB	�`B	�fB	�fB	�mB	�sB	�sB	�yB	�yB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B
  B
1B
oB
�B
#�B
1'B
6FB
=qB
B�B
H�B
O�B
T�B
[#B
]/B
cTB
gmB
k�B
n�B
q�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  BVBVB\BVBhBoB�B,B�hB�FBBĜBǮB��B�ZB��BoB�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B{BuB�B��B��B�B��BȴBǮB��B�mBBB1B%BBJBbBoB�B{BoB{B�B�B{B{B�BuBJB	7BbB\B\BPBJB��B�B�B�B�sB�/B��B��B��BǮBÖB�^B��B��BiyBXBR�BD�B2-B{BVB��B�;B��B�FB��B��B�PBq�BgmB]/BK�B8RB!�BPB
�B
�B
��B
��B
�=B
gmB
H�B
<jB
33B
.B
!�B
�B
B	�BB	��B	�LB	��B	�B	dZB	I�B	7LB	/B	 �B	�B	�B	B�B�`B�ZB�BB�BB�5B�5B�/B�TB�HB��B��B��B��BǮBǮBB�LB�B�B�?BȴB��B��B��B�?B�LB�?B�B��B��B��B��B��B��B��B��B�oB�bB�7B�%B�+B�VB�oB�uB�{B�=B�B~�Bw�Bw�Bx�Bu�By�By�Bw�Bn�Bn�Bp�BcTBcTBaHBaHB]/B[#B\)B[#BYBXBT�BT�BZBVBO�BL�BQ�BW
BYBVBS�BR�BN�BM�BL�BN�BL�BM�BC�BD�BF�B=qB8RB@�B5?B2-B49B1'B6FB/B-B.B+B,B/B2-B.B/B/B/B/B/B/B.B-B-B/B(�B(�B-B-B,B+B,B/B/B0!B2-B2-B1'B/B1'B2-B2-B2-B2-B6FB8RB:^B:^B;dB<jB?}BE�BC�BG�BG�BI�BYBO�B[#BZB[#BcTBffBffBgmBk�Bo�Bp�Bt�Bx�B{�B~�B�1B�+B�7B�=B�DB�JB�\B�uB��B��B��B��B��B��B��B��B��B�B�!B�9B�FB�XB�}BĜBŢBƨB��B�#B�NB�TB�`B�yB�B�B�B��B	B��B��B	
=B	DB	PB	bB	oB	{B	�B	�B	�B	�B	�B	�B	�B	"�B	&�B	+B	'�B	'�B	0!B	6FB	7LB	;dB	=qB	H�B	I�B	G�B	E�B	F�B	F�B	P�B	T�B	T�B	K�B	N�B	`BB	bNB	e`B	iyB	l�B	q�B	t�B	x�B	z�B	|�B	� B	�B	�B	�1B	�7B	�=B	�=B	�VB	�hB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�!B	�!B	�9B	�-B	�'B	�'B	�-B	�9B	�FB	�FB	�LB	�LB	�RB	�^B	�qB	�wB	�wB	�}B	��B	B	ÖB	B	ÖB	ĜB	ȴB	ȴB	ȴB	ȴB	ȴB	ɺB	ɺB	ɺB	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�
B	�B	�B	�B	�B	�#B	�#B	�/B	�5B	�;B	�;B	�;B	�BB	�BB	�BB	�BB	�HB	�HB	�HB	�HB	�TB	�TB	�ZB	�ZB	�fB	�fB	�mB	�sB	�sB	�sB	�yB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B
  B
	7B
oB
�B
$�B
1'B
6FB
=qB
B�B
H�B
O�B
T�B
[#B
]/B
cTB
gmB
k�B
n�B
r�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  <#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
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
PRES            TEMP            PSAL            PRES_ADJUSTED = PRES - dP                                                                                                                                                                                                                                       none                                                                                                                                                                                                                                                            PSAL_ADJUSTED = sw_salt( sw_cndr(PSAL,TEMP,PRES), TEMP, PRES_ADJUSTED ) + CTM                                                                                                                                                                                   dP =0.4 dbar.                                                                                                                                                                                                                                                   none                                                                                                                                                                                                                                                            CTM: alpha=0.141, tau=6.68s                                                                                                                                                                                                                                     Pressures adjusted by using pressure offset at the sea surface. The quoted error is manufacturer specified accuracy in dbar.                                                                                                                                    The quoted error is manufacturer specified accuracy with respect to ITS-90 at time of laboratory calibration.                                                                                                                                                   No significant salinity drift detected. Salinity adjusted for effects of pressure adjustment. Conductivity cell thermal mass correction (CTM) applied to measurements taken in Cp mode. The quoted error is max[0.01, sqrt(OWerr^2+CTMerr^2)] in PSS-78.        201201101452202012011014522020120110145220  AO  ARGQ                                                                        20111130144309  QCP$                G�O�G�O�G�O�FFBFE           AO  ARGQ                                                                        20111130144309  QCF$                G�O�G�O�G�O�0               UW  ARSQUWQC    WOD & nearby Argo as visual check                               20120110145220  IP                  G�O�G�O�G�O�                