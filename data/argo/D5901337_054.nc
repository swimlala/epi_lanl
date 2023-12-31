CDF      
      	DATE_TIME         STRING2       STRING4       STRING8       STRING16      STRING32       STRING64   @   	STRING256         N_PROF        N_PARAM       N_LEVELS     N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       D2011-12-05 AOML 2.2 creation; 2015-12-13T23:15:38Z UW 3.1 conversion   
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
_FillValue                    ��Argo profile    3.1 1.2 19500101000000  5901337 US ARGO PROJECT                                                 Stephen Riser                                                   PRES            TEMP            PSAL               6A   AO  20111205113233  20190522121836  1901_5055_054                   2C  D   APEX                            2140                            040306                          846 @ԯ�h|�
1   @ԯ�W?�@-W�O�;d�cq�^5?}1   GPS     Primary sampling: mixed [deeper than nominal 990dbar: discrete; nominal 990dbar to surface: 2dbar-bin averaged]                                                                                                                                                    A   A   A   @�33@�  A��A   A>ffA`  A�  A�33A�33A�  A�  A�  A�  A�  B   B  B  B  B   B(  B0  B8  B@  BH  BP  BX  B`  Bh  Bp  BxffB���B�  B���B�  B�  B�ffB���B�  B�  B���B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�33B�  B�  B�  B�  B�  B�  B���B�  B�  C   C  C  C  C  C
  C  C  C  C  C�C  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4�C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C�  C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D�fD  D� D  D� D��D� D  D� D  D� D  D� D  D� D��D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,�fD-  D-y�D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� DffDf�fDg  Dg� Dh  Dh� Di  Diy�Dj  Dj�fDk  Dk� DlfDl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dt� Du  Du� Dv  Dv� Dw  Dw� D��3D�C3D��3D��fD���D�,�D�\�D���D��D�<�D���D��3D���D�)�Dډ�D�3D���D��D�\�D�#31111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  @l��@�33@�ffA��A8  AY��Ay��A�  A�  A���A���A���A���A���A���BffBffBffBffB&ffB.ffB6ffB>ffBFffBNffBVffB^ffBfffBnffBv��B��B�33B�  B�33B�33B���B�  B�33B�33B�  B�33B�33B�33B�33B�33B�33B�33B�33B�33B�33B�33B�33B�ffB�33B�33B�33B�33B�33B�33B�  B�33B�33B�33C��C��C��C��C	��C��C��C��C��C�3C��C��C��C��C��C��C!��C#��C%��C'��C)��C+��C-��C/��C1��C3�3C5��C7��C9��C;��C=��C?��CA��CC��CE��CG��CI��CK��CM��CO��CQ��CS��CU��CW��CY��C[��C]��C_��Ca��Cc��Ce��Cg��Ci��Ck��Cm��Co��Cq��Cs��Cu��Cw��Cy��C{��C}��C��C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C�� C�� C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C�� C���C�� C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���D ffD �fDffD�fDffD�fDffD�fDffD�fDffD�fDffD�fDffD�fDffD�fD	ffD	�fD
ffD
�fDffD�fDffD�fDffD�fDffD�fDffD�fDffD�fDl�D�fDffD�fDffD� DffD�fDffD�fDffD�fDffD�fDffD� DffD�fDffD�fDffD�fDffD�fDffD�fDffD�fDffD�fD ffD �fD!ffD!�fD"ffD"�fD#ffD#�fD$ffD$�fD%ffD%�fD&ffD&�fD'ffD'�fD(ffD(�fD)ffD)�fD*ffD*�fD+ffD+�fD,l�D,�fD-` D-�fD.ffD.�fD/ffD/�fD0ffD0�fD1ffD1�fD2ffD2�fD3ffD3�fD4ffD4�fD5ffD5�fD6ffD6�fD7ffD7�fD8ffD8�fD9ffD9�fD:ffD:�fD;ffD;�fD<ffD<�fD=ffD=�fD>ffD>�fD?ffD?�fD@ffD@�fDAffDA�fDBffDB�fDCffDC�fDDffDD�fDEffDE�fDFffDF�fDGffDG�fDHffDH�fDIffDI�fDJffDJ�fDKffDK�fDLffDL�fDMffDM�fDNffDN�fDOffDO�fDPffDP�fDQffDQ�fDRffDR�fDSffDS�fDTffDT�fDUffDU�fDVffDV�fDWffDW�fDXffDX�fDYffDY�fDZffDZ�fD[ffD[�fD\ffD\�fD]ffD]�fD^ffD^�fD_ffD_�fD`ffD`�fDaffDa�fDbffDb�fDcffDc�fDdffDd�fDeffDe��Dfl�Df�fDgffDg�fDhffDh�fDi` Di�fDjl�Dj�fDkffDk��DlffDl�fDmffDm�fDnffDn�fDoffDo�fDpffDp�fDqffDq�fDrffDr�fDsffDs�fDtffDt�fDuffDu�fDvffDv�fDwffD��fD�6fD�vfD�ɚD�� D�  D�P D���D�  D�0 D�� DǶfD���D��D�|�D��fD�� D� D�P D�f1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  @��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��A˓uA˕�A˗�A˛�A˝�A˙�A˧�A˩�A˩�AˬA˰!A˲-A˴9A˲-A˴9A˶FA˶FA˶FA˸RA˺^A˺^A˺^A˸RA˸RA˥�A�x�A�\)A�I�A�G�A�E�A�?}A�(�Aʕ�A�dZA�?}A� �A��`A�7LA��A�1A��`A���A��uA��9A�A��A��A�9XA�"�A�{A�/A� �A�hsA��7A��/A���A���A�t�A��yA��A�r�A�x�A���A�~�A���A���A�33A�9XA�M�A��A�Q�A���A�?}A�K�A�%A��HA�7LA�l�A�VA�+A�|�A��;A� �A~��A{��Aw�Au�Au|�Ap�Aj�9Af�`Aa7LA\�/AY�AVn�AS�;AQ��AO��AN�RAMp�AJn�AG;dAE|�A@�A9�A8�A7+A4�yA4{A3x�A2��A1�FA0bNA.�9A-��A-;dA,�!A+�FA*z�A)��A*A)�A)��A)x�A)l�A)`BA)S�A)&�A)A)A(��A(�yA(�!A(^5A(bNA(r�A'�A'�-A'S�A&�`A&�`A&��A&��A&�!A&�A$�\A#ƨA#A"r�A"A!��A!&�A �RA   A33A�`A�uA��A�`A��An�AE�A��A�wA�yA��Az�A1'A�A��A\)AA��A��A�A��Av�A5?A1'A�A�TA�TA�A�A�AĜA9XA�A��AO�Av�A�A�wAdZA"�A�!A9XA�PA"�A��A��A��A�A�AAx�A\)A�AȴA�!AjA��A�A
��A
n�A
ffA
M�A
1A	|�A~�A��A��A��Al�AC�A7LA33A�A��A�+AM�A{AƨA&�A��AA�A1A�A��A�AXA�A�Ar�A  A ��A -@���@�+@�J@�C�@��^@���@�  @�=q@��@���@�-@�&�@�(�@�\)@���@��`@��@�F@�33@�"�@�
=@�V@��@��/@�|�@��@�@�Ĝ@�bN@�  @��@��y@�-@�9@�9X@��@�n�@�hs@�j@���@�"�@١�@�I�@���@ץ�@ׅ@֟�@�E�@��@�1@�dZ@�+@��@�ȴ@���@җ�@�@�/@Гu@ϝ�@�S�@�"�@���@���@́@�X@�/@̬@��
@ˍP@�33@�J@ə�@�X@�%@�bN@��m@Ǿw@ǝ�@�@�~�@�$�@Ų-@�X@�/@��/@ċD@�Z@�Q�@��@�
=@¸R@+@�$�@��@���@���@�ȴ@�M�@��@���@�@���@�O�@��@��/@�Ĝ@���@�Z@�b@��@�;d@�n�@��@��7@�/@��@�ƨ@��@���@��7@��9@�z�@�  @��F@��F@�S�@�@���@���@���@���@���@���@��+@�v�@�M�@�@�O�@�G�@�?}@�V@�(�@�1@�1@��@�C�@���@�5?@�@�X@��@�I�@��w@��@�{@��@���@�@���@�7L@�Z@�  @�33@��y@���@�5?@�J@��T@��-@�O�@�V@��@���@���@��@��;@�S�@�K�@�;d@�33@�+@�33@�@��\@�M�@��@���@�hs@�?}@�V@��@���@��D@�I�@�b@��@�l�@��R@���@��\@�^5@�{@��@��#@��@�V@��j@�bN@�Q�@�1'@��F@�+@�@��y@���@���@��R@��!@���@�ff@�=q@�{@��@�@�x�@���@���@��u@�r�@�Q�@�b@���@��
@��@��@�K�@��@��!@�~�@�^5@�=q@��@�p�@�/@�%@��@�-@�ff@��j@r�!@i�7@c�F@[�m@U��@L��@Co@;�@4�@-/@(1'@!��@5?@��@�m@v�@	��1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  A˓uA˕�A˗�A˛�A˝�A˙�A˧�A˩�A˩�AˬA˰!A˲-A˴9A˲-A˴9A˶FA˶FA˶FA˸RA˺^A˺^A˺^A˸RA˸RA˥�A�x�A�\)A�I�A�G�A�E�A�?}A�(�Aʕ�A�dZA�?}A� �A��`A�7LA��A�1A��`A���A��uA��9A�A��A��A�9XA�"�A�{A�/A� �A�hsA��7A��/A���A���A�t�A��yA��A�r�A�x�A���A�~�A���A���A�33A�9XA�M�A��A�Q�A���A�?}A�K�A�%A��HA�7LA�l�A�VA�+A�|�A��;A� �A~��A{��Aw�Au�Au|�Ap�Aj�9Af�`Aa7LA\�/AY�AVn�AS�;AQ��AO��AN�RAMp�AJn�AG;dAE|�A@�A9�A8�A7+A4�yA4{A3x�A2��A1�FA0bNA.�9A-��A-;dA,�!A+�FA*z�A)��A*A)�A)��A)x�A)l�A)`BA)S�A)&�A)A)A(��A(�yA(�!A(^5A(bNA(r�A'�A'�-A'S�A&�`A&�`A&��A&��A&�!A&�A$�\A#ƨA#A"r�A"A!��A!&�A �RA   A33A�`A�uA��A�`A��An�AE�A��A�wA�yA��Az�A1'A�A��A\)AA��A��A�A��Av�A5?A1'A�A�TA�TA�A�A�AĜA9XA�A��AO�Av�A�A�wAdZA"�A�!A9XA�PA"�A��A��A��A�A�AAx�A\)A�AȴA�!AjA��A�A
��A
n�A
ffA
M�A
1A	|�A~�A��A��A��Al�AC�A7LA33A�A��A�+AM�A{AƨA&�A��AA�A1A�A��A�AXA�A�Ar�A  A ��A -@���@�+@�J@�C�@��^@���@�  @�=q@��@���@�-@�&�@�(�@�\)@���@��`@��@�F@�33@�"�@�
=@�V@��@��/@�|�@��@�@�Ĝ@�bN@�  @��@��y@�-@�9@�9X@��@�n�@�hs@�j@���@�"�@١�@�I�@���@ץ�@ׅ@֟�@�E�@��@�1@�dZ@�+@��@�ȴ@���@җ�@�@�/@Гu@ϝ�@�S�@�"�@���@���@́@�X@�/@̬@��
@ˍP@�33@�J@ə�@�X@�%@�bN@��m@Ǿw@ǝ�@�@�~�@�$�@Ų-@�X@�/@��/@ċD@�Z@�Q�@��@�
=@¸R@+@�$�@��@���@���@�ȴ@�M�@��@���@�@���@�O�@��@��/@�Ĝ@���@�Z@�b@��@�;d@�n�@��@��7@�/@��@�ƨ@��@���@��7@��9@�z�@�  @��F@��F@�S�@�@���@���@���@���@���@���@��+@�v�@�M�@�@�O�@�G�@�?}@�V@�(�@�1@�1@��@�C�@���@�5?@�@�X@��@�I�@��w@��@�{@��@���@�@���@�7L@�Z@�  @�33@��y@���@�5?@�J@��T@��-@�O�@�V@��@���@���@��@��;@�S�@�K�@�;d@�33@�+@�33@�@��\@�M�@��@���@�hs@�?}@�V@��@���@��D@�I�@�b@��@�l�@��R@���@��\@�^5@�{@��@��#@��@�V@��j@�bN@�Q�@�1'@��F@�+@�@��y@���@���@��R@��!@���@�ff@�=q@�{@��@�@�x�@���@���@��u@�r�@�Q�@�b@���@��
@��@��@�K�@��@��!@�~�@�^5@�=q@��@�p�@�/@�%@��@�-@�ff@��j@r�!@i�7@c�F@[�m@U��@L��@Co@;�@4�@-/@(1'@!��@5?@��@�m@v�@	��1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  ;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oB	iyB	iyB	iyB	iyB	iyB	iyB	iyB	iyB	iyB	iyB	jB	k�B	jB	k�B	jB	jB	k�B	l�B	k�B	jB	jB	n�B	v�B	~�B	��B	�ZB
=qB
n�B
r�B
t�B
x�B
�B
�BBdZB�B�%B�=B�{B��B��B��B��B�}B�^B�XBŢB�B�B�B��B��B
=B  BBVB�B;dBA�B2-B�B/B:^B0!B"�B�B	7B�B��B�FB��B~�BB�B�BB
�B
�HB
��B
��B
gmB
?}B
#�B

=B	��B	�`B	��B	��B	�?B	�7B	iyB	M�B	0!B	�B	1B��B�B�sB�NB�5B�B��B��B��BɺB�B�NB�`B��B��B	B	B	
=B	\B	bB	�B	'�B	H�B	M�B	R�B	]/B	bNB	p�B	��B	��B	�?B	ÖB	�B	�TB	�B	�B	��B	��B
B
PB
\B
{B
�B
�B
�B
�B
&�B
)�B
/B
6FB
9XB
6FB
6FB
6FB
6FB
5?B
5?B
6FB
5?B
2-B
2-B
1'B
2-B
2-B
/B
.B
.B
.B
.B
.B
/B
/B
/B
/B
.B
.B
/B
33B
33B
49B
8RB
=qB
=qB
A�B
C�B
C�B
F�B
N�B
J�B
K�B
O�B
M�B
N�B
N�B
N�B
O�B
Q�B
P�B
P�B
O�B
M�B
K�B
J�B
K�B
I�B
H�B
F�B
D�B
A�B
>wB
=qB
<jB
<jB
;dB
9XB
8RB
5?B
33B
1'B
0!B
/B
.B
-B
+B
'�B
$�B
$�B
&�B
'�B
)�B
+B
+B
+B
)�B
(�B
'�B
&�B
%�B
#�B
#�B
"�B
$�B
%�B
%�B
%�B
$�B
'�B
'�B
(�B
'�B
$�B
�B
�B
�B
�B
�B
uB
\B
PB
DB
	7B
	7B
1B
1B
+B
+B
B
B
B
B
B
B
B
  B	��B
  B
B	��B	��B
  B
  B
  B
  B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
  B
  B
  B
  B
B
B
B
B
B
B
B
B
B
B
+B
+B
+B
%B
%B
%B
+B
1B
1B
1B
1B
1B
1B
	7B
	7B
	7B
	7B
	7B
1B
+B
+B
+B
+B
%B
+B
+B
+B
1B
	7B
	7B

=B
DB
JB
VB
PB
VB
VB
VB
\B
bB
bB
bB
bB
bB
bB
hB
hB
hB
bB
bB
bB
bB
\B
\B
\B
\B
VB
VB
VB
VB
VB
\B
\B
hB
hB
hB
hB
hB
hB
hB
oB
oB
oB
oB
oB
uB
uB
uB
uB
uB
uB
uB
uB
uB
uB
{B
{B
{B
{B
{B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
"�B
(�B
.B
8RB
?}B
C�B
G�B
M�B
R�B
XB
^5B
cTB
ffB
jB
o�B
q�B
t�B
y�B
� B
�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  B	iyB	iyB	iyB	iyB	iyB	iyB	iyB	iyB	iyB	iyB	jB	k�B	jB	k�B	jB	jB	k�B	l�B	k�B	jB	jB	n�B	v�B	� B	��B	�`B
=qB
n�B
r�B
t�B
x�B
�B
�'B
=BjB�%B�DB�\B��B��B�B��B��BȴBƨB�}BɺB��B�B�)B�HB  BbBBBVB �B>wBE�B:^B$�B33BA�B5?B&�B�BuB��B��B�^B��B�PBM�B�B%B
�B
�ZB
�B
�FB
v�B
H�B
/B
bB
B	�B	��B	ÖB	ǮB	��B	w�B	aHB	?}B	$�B	{B	B��B�B�fB�TB�TB�5B�B�B�)B�;B�`B�B��B	B	B	1B	VB	{B	uB	�B	)�B	K�B	P�B	S�B	]/B	bNB	q�B	��B	��B	�?B	ÖB	�B	�ZB	�B	�B	��B	��B
B
PB
\B
�B
�B
�B
�B
�B
'�B
+B
/B
7LB
?}B
8RB
8RB
8RB
7LB
6FB
6FB
7LB
7LB
49B
33B
2-B
49B
5?B
0!B
/B
/B
/B
/B
0!B
0!B
0!B
0!B
0!B
/B
/B
0!B
33B
33B
5?B
9XB
>wB
>wB
A�B
C�B
D�B
F�B
P�B
K�B
K�B
P�B
O�B
O�B
O�B
O�B
R�B
R�B
Q�B
Q�B
P�B
O�B
M�B
L�B
M�B
K�B
J�B
G�B
F�B
D�B
?}B
>wB
=qB
=qB
<jB
:^B
9XB
7LB
6FB
33B
1'B
/B
/B
.B
-B
,B
&�B
%�B
'�B
(�B
+B
+B
+B
,B
+B
)�B
(�B
'�B
&�B
%�B
$�B
$�B
%�B
&�B
&�B
&�B
%�B
(�B
(�B
+B
)�B
)�B
!�B
�B
�B
�B
�B
�B
hB
\B
VB
DB
DB
DB

=B
	7B
	7B
1B
B
B
B
B
B
B
B
  B
B
B
B
B
B
B
B
B
  B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
  B
  B
B
  B
B
B
B
B
B
B
B
B
%B
+B
+B
+B
+B
+B
+B
+B
1B
	7B
	7B
	7B
	7B

=B

=B
	7B
	7B
	7B
	7B

=B

=B
1B
1B
1B
1B
+B
+B
+B
+B
	7B

=B

=B

=B
DB
JB
\B
VB
VB
VB
VB
\B
bB
hB
hB
hB
hB
hB
oB
oB
oB
bB
bB
hB
hB
bB
bB
bB
bB
VB
VB
\B
\B
VB
\B
bB
oB
oB
oB
hB
oB
oB
hB
oB
oB
oB
oB
oB
uB
uB
{B
uB
{B
{B
uB
{B
{B
{B
{B
{B
�B
�B
{B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
"�B
(�B
/B
9XB
?}B
C�B
G�B
M�B
S�B
XB
^5B
cTB
gmB
jB
o�B
q�B
t�B
z�B
� B
�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  <#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
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
<e`B<#�
<#�
<#�
<#�
<#�
<49X<���<u<#�
<49X<#�
<#�
<e`B<#�
<#�
<�t�<�t�<e`B<���<u<49X<49X<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<D��<�C�<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
PRES            TEMP            PSAL            PRES_ADJUSTED = PRES - dP                                                                                                                                                                                                                                       none                                                                                                                                                                                                                                                            PSAL_ADJUSTED = sw_salt( sw_cndr(PSAL,TEMP,PRES), TEMP, PRES_ADJUSTED ) + CTM                                                                                                                                                                                   dP =0.4 dbar.                                                                                                                                                                                                                                                   none                                                                                                                                                                                                                                                            CTM: alpha=0.141, tau=6.68s                                                                                                                                                                                                                                     Pressures adjusted by using pressure offset at the sea surface. The quoted error is manufacturer specified accuracy in dbar.                                                                                                                                    The quoted error is manufacturer specified accuracy with respect to ITS-90 at time of laboratory calibration.                                                                                                                                                   No significant salinity drift detected. Salinity adjusted for effects of pressure adjustment. Conductivity cell thermal mass correction (CTM) applied to measurements taken in Cp mode. The quoted error is max[0.01, sqrt(OWerr^2+CTMerr^2)] in PSS-78.        201201131250262012011312502620120113125026  AO  ARGQ                                                                        20111205113233  QCP$                G�O�G�O�G�O�FFBFE           AO  ARGQ                                                                        20111205113233  QCF$                G�O�G�O�G�O�0               UW  ARSQUWQC    WOD & nearby Argo as visual check                               20120113125026  IP                  G�O�G�O�G�O�                