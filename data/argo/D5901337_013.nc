CDF      
      	DATE_TIME         STRING2       STRING4       STRING8       STRING16      STRING32       STRING64   @   	STRING256         N_PROF        N_PARAM       N_LEVELS     N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       D2011-12-05 AOML 2.2 creation; 2015-12-13T23:15:27Z UW 3.1 conversion   
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
_FillValue                    ��Argo profile    3.1 1.2 19500101000000  5901337 US ARGO PROJECT                                                 Stephen Riser                                                   PRES            TEMP            PSAL               A   AO  20111205112511  20190522121836  1901_5055_013                   2C  D   APEX                            2140                            040306                          846 @�I��f�1   @�I�� @.��x����cr�t�1   GPS     Primary sampling: mixed [deeper than nominal 990dbar: discrete; nominal 990dbar to surface: 2dbar-bin averaged]                                                                                                                                                    A   A   A   @���@�  A   A   A@  A`  A���A�  A�  A�  A�  A�  A�  A�  B   B  B  B  B   B(  B0  B8  B@  BH  BPffBW33B_��Bh  Bp  Bx  B�  B�  B�  B�ffB���B���B���B���B�  B�33B�33B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�33B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  C   C  C  C  C  C
  C  C  C  C  C  C  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C1�fC4  C6  C8  C:  C<  C>  C?�fCB  CD  CF�CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C{�fC~  C�  C�  C�  C�  C�  C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D��Dy�D��D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D��D� D  Dy�D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D>��D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DKfDK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Dby�Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh�fDifDi� Dj  Dj� Dk  Dk�fDl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dt� Du  Du� Dv  Dv� Dw  Dy� D���D�FfD�p D��3D��fD�0 D�� D��3D��3D�,�D�s3D��fD�  D�  D�Y�D๚D��3D��D�C3D�31111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  @y��@�33@�33A��A9��AY��A{33A���A���A���A���A���A���A���A���BffBffBffBffB&ffB.ffB6ffB>ffBFffBN��BU��B^  BfffBnffBvffB~ffB�33B�33B���B���B�  B�  B�  B�33B�ffB�ffB�33B�33B�33B�33B�33B�33B�33B�33B�33B�33B�ffB�33B�33B�33B�33B�33B�33B�33B�33B�33B�33B�33C��C��C��C��C	��C��C��C��C��C��C��C��C��C��C��C��C!��C#��C%��C'��C)��C+��C-��C/��C1� C3��C5��C7��C9��C;��C=��C?� CA��CC��CE�3CG��CI��CK��CM��CO��CQ��CS��CU��CW��CY��C[��C]��C_��Ca��Cc��Ce��Cg��Ci��Ck��Cm��Co��Cq��Cs��Cu��Cw��Cy��C{� C}��C��C���C���C���C���C�� C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C�� C���C���C���C���C���C���C���C���C���C���C���C���C���C���C�� C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���D ffD �fDffD�fDffD� D` D� DffD�fDffD�fDffD�fDffD�fDffD�fD	ffD	�fD
ffD
�fDffD�fDffD�fDffD�fDffD�fDffD�fDffD�fDffD�fDffD�fDffD�fDffD�fDffD�fDffD�fDffD�fDffD�fDffD�fDffD�fDffD�fDffD� DffD�fD` D�fDffD�fD ffD �fD!ffD!�fD"ffD"�fD#ffD#�fD$ffD$�fD%ffD%�fD&ffD&�fD'ffD'�fD(ffD(�fD)ffD)�fD*ffD*�fD+ffD+�fD,ffD,�fD-ffD-�fD.ffD.�fD/ffD/�fD0ffD0�fD1ffD1�fD2ffD2�fD3ffD3�fD4ffD4�fD5ffD5�fD6ffD6�fD7ffD7�fD8ffD8�fD9ffD9�fD:ffD:�fD;ffD;�fD<ffD<�fD=ffD=�fD>ffD>� D?ffD?�fD@ffD@�fDAffDA�fDBffDB�fDCffDC�fDDffDD�fDEffDE�fDFffDF�fDGffDG�fDHffDH�fDIffDI�fDJffDJ��DKffDK�fDLffDL�fDMffDM�fDNffDN�fDOffDO�fDPffDP�fDQffDQ�fDRffDR�fDSffDS�fDTffDT�fDUffDU�fDVffDV�fDWffDW�fDXffDX�fDYffDY�fDZffDZ�fD[ffD[�fD\ffD\�fD]ffD]�fD^ffD^�fD_ffD_�fD`ffD`�fDaffDa�fDb` Db�fDcffDc�fDdffDd�fDeffDe�fDfffDf�fDgffDg�fDhl�Dh��DiffDi�fDjffDj�fDkl�Dk�fDlffDl�fDmffDm�fDnffDn�fDoffDo�fDpffDp�fDqffDq�fDrffDr�fDsffDs�fDtffDt�fDuffDu�fDvffDv�fDyffD�� D�9�D�c3D��fD��D�#3D�s3D��fD��fD�  D�ffDǹ�D��3D�3D�L�D��D��fD� D�6fD��f1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  @��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��A��A��A��yA��TA��`A��HA��HA��HA��;A��
A��A��
A�ƨA�ȴA�ȴA�ȴA�ȴA�ƨA�ƨA�ĜA���AپwAٶFA٧�Aى7A���A�|�Aհ!A��mAԸRA�-A�n�A�r�A�O�A�(�A�bA�$�A��^A�~�A��A��A�l�A�XA��A�C�A��wA�9XA���A���A�oA���A��7A��RA���A�C�A���A��7A�VA��mA��A�z�A��+A��\A���A��
A�&�A��DA�5?A��#A�$�A��A���A���A��-A�9XA���A��A�ĜA��
A��hA�z�A�G�A���A��A}��Aw��Au+Ap��Am�FAl9XAg/AbffA^��A\��AYXAW?}ATĜAQ��AN�ANA�AL�`AHz�AD  A@v�A=A<1A:�A9�A:^5A9��A9�wA9�#A:JA9��A9VA6(�A1�^A/�A.=qA.A�A.5?A-��A-/A-oA,��A,�/A+�-A)33A&��A"jA��Av�AƨA�9A�AE�A��A�A��A�A-A&�A�\A�hAjA��A��AI�A"�An�A�PA7LAȴA��A33A^5A33AVA�At�A��A��A��AZA5?A�A;dA ��A n�A A�@���@���@�%@�  @���@��h@��`@�Ĝ@�  @�=q@��-@�X@���@�z�@��
@���@�J@�@��@�z�@�(�@�F@��y@��@�O�@��@�l�@���@�%@�(�@�9X@�r�@�j@��@��@���@�l�@�33@�~�@��@�I�@��
@�"�@�~�@�@���@ݙ�@�?}@�%@�Ĝ@�Z@��@�;d@���@�$�@���@���@ٙ�@ش9@���@��@��;@ם�@��@֧�@֏\@�~�@�n�@��@պ^@թ�@Ցh@�`B@Չ7@���@�$�@�E�@�x�@�9X@��@ӥ�@ӕ�@Ӿw@�
=@�"�@��@�@��`@�(�@�C�@��H@�~�@��#@�j@�K�@��@�n�@ɑh@ț�@ǶF@�C�@�;d@�ff@�p�@���@ě�@�ƨ@�@�5?@�?}@�1'@�dZ@�
=@��@���@��\@�~�@��+@�ff@�5?@��@���@�V@�Ĝ@��u@��;@���@�5?@�{@��7@��@��@���@�z�@� �@��;@�K�@��R@�M�@�E�@��T@�X@�V@�%@�Ĝ@�b@�K�@���@���@�v�@�ff@�M�@�-@��T@�?}@���@���@���@��j@���@�bN@�b@��;@��@���@�l�@�;d@��R@���@���@�n�@��-@�%@��@��@�z�@�1'@���@���@��@��@��y@��H@���@�n�@�E�@�5?@�J@���@��@��@��D@�1@��;@��w@���@��@���@�v�@�-@�@��#@�x�@��@��9@�r�@�I�@���@���@���@�+@��@���@�=q@���@�O�@���@���@�Z@�9X@��@�  @�ƨ@���@�dZ@�o@��y@���@��@�p�@�/@���@���@�A�@�1@���@�|�@�33@��@���@�n�@�M�@�{@��T@�p�@�V@��/@��D@�(�@���@�"�@��@���@�E�@��-@�?}@���@�Z@�(�@�|�@�"�@���@�V@���@���@�O�@���@�r�@�9X@���@��F@���@��@�|�@�|�@�dZ@�;d@��@��!@���@�v�@�V@�E�@�=q@��@�p�@��@�Ĝ@��9@�r�@�9X@�b@��@��@� �@��@��
@�K�@�
=@�@��y@��R@�ff@�$�@�@���@�`B@�?}@�`B@���@��h@�x�@�7L@���@�1'@�1'@�b@��@��;@���@��w@�dZ@��\@�@��;@z~�@l�@e�@Z~�@R^5@J��@D�D@<��@5@/�w@)�^@#�
@{@�7@��@r�@�@	��@v�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  A��A��A��yA��TA��`A��HA��HA��HA��;A��
A��A��
A�ƨA�ȴA�ȴA�ȴA�ȴA�ƨA�ƨA�ĜA���AپwAٶFA٧�Aى7A���A�|�Aհ!A��mAԸRA�-A�n�A�r�A�O�A�(�A�bA�$�A��^A�~�A��A��A�l�A�XA��A�C�A��wA�9XA���A���A�oA���A��7A��RA���A�C�A���A��7A�VA��mA��A�z�A��+A��\A���A��
A�&�A��DA�5?A��#A�$�A��A���A���A��-A�9XA���A��A�ĜA��
A��hA�z�A�G�A���A��A}��Aw��Au+Ap��Am�FAl9XAg/AbffA^��A\��AYXAW?}ATĜAQ��AN�ANA�AL�`AHz�AD  A@v�A=A<1A:�A9�A:^5A9��A9�wA9�#A:JA9��A9VA6(�A1�^A/�A.=qA.A�A.5?A-��A-/A-oA,��A,�/A+�-A)33A&��A"jA��Av�AƨA�9A�AE�A��A�A��A�A-A&�A�\A�hAjA��A��AI�A"�An�A�PA7LAȴA��A33A^5A33AVA�At�A��A��A��AZA5?A�A;dA ��A n�A A�@���@���@�%@�  @���@��h@��`@�Ĝ@�  @�=q@��-@�X@���@�z�@��
@���@�J@�@��@�z�@�(�@�F@��y@��@�O�@��@�l�@���@�%@�(�@�9X@�r�@�j@��@��@���@�l�@�33@�~�@��@�I�@��
@�"�@�~�@�@���@ݙ�@�?}@�%@�Ĝ@�Z@��@�;d@���@�$�@���@���@ٙ�@ش9@���@��@��;@ם�@��@֧�@֏\@�~�@�n�@��@պ^@թ�@Ցh@�`B@Չ7@���@�$�@�E�@�x�@�9X@��@ӥ�@ӕ�@Ӿw@�
=@�"�@��@�@��`@�(�@�C�@��H@�~�@��#@�j@�K�@��@�n�@ɑh@ț�@ǶF@�C�@�;d@�ff@�p�@���@ě�@�ƨ@�@�5?@�?}@�1'@�dZ@�
=@��@���@��\@�~�@��+@�ff@�5?@��@���@�V@�Ĝ@��u@��;@���@�5?@�{@��7@��@��@���@�z�@� �@��;@�K�@��R@�M�@�E�@��T@�X@�V@�%@�Ĝ@�b@�K�@���@���@�v�@�ff@�M�@�-@��T@�?}@���@���@���@��j@���@�bN@�b@��;@��@���@�l�@�;d@��R@���@���@�n�@��-@�%@��@��@�z�@�1'@���@���@��@��@��y@��H@���@�n�@�E�@�5?@�J@���@��@��@��D@�1@��;@��w@���@��@���@�v�@�-@�@��#@�x�@��@��9@�r�@�I�@���@���@���@�+@��@���@�=q@���@�O�@���@���@�Z@�9X@��@�  @�ƨ@���@�dZ@�o@��y@���@��@�p�@�/@���@���@�A�@�1@���@�|�@�33@��@���@�n�@�M�@�{@��T@�p�@�V@��/@��D@�(�@���@�"�@��@���@�E�@��-@�?}@���@�Z@�(�@�|�@�"�@���@�V@���@���@�O�@���@�r�@�9X@���@��F@���@��@�|�@�|�@�dZ@�;d@��@��!@���@�v�@�V@�E�@�=q@��@�p�@��@�Ĝ@��9@�r�@�9X@�b@��@��@� �@��@��
@�K�@�
=@�@��y@��R@�ff@�$�@�@���@�`B@�?}@�`B@���@��h@�x�@�7L@���@�1'@�1'@�b@��@��;@���@��w@�dZ@��\@�@��;@z~�@l�@e�@Z~�@R^5@J��@D�D@<��@5@/�w@)�^@#�
@{@�7@��@r�@�@	��@v�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  ;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oB	u�B	u�B	t�B	u�B	t�B	u�B	t�B	t�B	t�B	t�B	u�B	t�B	t�B	t�B	t�B	t�B	t�B	t�B	t�B	t�B	t�B	t�B	u�B	v�B	|�B	�FB
�B
x�B
�3B
ɺB
��B
�^B
��B
e`B
T�B
�+B
��B
�ZB
�-B
��B
�qB
��B
�B
�B
�mB
ƨB
�'B
�{B
�B
ŢB
�HBB
��B
�B
�B
��B
��BBB�B9XBD�B<jB#�BPB
�ZB
ƨB
�LB
��B
r�B
[#B
G�B
)�B
�B
VB
!�B
(�B
t�B
~�B
w�B
T�B
1'B
	7B	�qB	��B	t�B	O�B	>wB	)�B	 �B	�B��B�mB�
B��B��B�XB�3B�B�B�!B�B�B��B��B��B��B�}B��B�ZB�B	DB	&�B	I�B	ffB	t�B	_;B	H�B	8RB	33B	8RB	;dB	?}B	VB	YB	XB	T�B	I�B	6FB	 �B	B�fB�B�B	B	�B	�B	&�B	�B	
=B��B�B�B�B�B�yB�yB�B�B�B�yB�B��B	#�B	L�B	[#B	v�B	y�B	v�B	p�B	[#B	E�B	.B	�B	�B	 �B	�B	$�B	%�B	)�B	+B	0!B	49B	33B	1'B	1'B	0!B	33B	7LB	;dB	;dB	<jB	>wB	>wB	?}B	?}B	C�B	H�B	J�B	L�B	M�B	N�B	P�B	R�B	W
B	ZB	bNB	jB	jB	m�B	p�B	v�B	|�B	�B	�B	�%B	}�B	|�B	|�B	z�B	y�B	{�B	|�B	~�B	� B	�B	�B	�B	�B	�B	�B	�B	�%B	�+B	�7B	�DB	�DB	�DB	�DB	�\B	�oB	�uB	�{B	�{B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	��B	��B	�B	�B	�B	�!B	�?B	�9B	�3B	�9B	�3B	�-B	�-B	�'B	�B	�B	�B	�B	�B	�B	�B	�!B	�!B	�9B	�?B	�RB	�^B	�dB	�dB	�jB	�jB	�jB	�qB	�wB	�}B	��B	ĜB	ŢB	ȴB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	ɺB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�
B	�B	�B	�B	�)B	�5B	�HB	�ZB	�ZB	�TB	�fB	�sB	�sB	�mB	�sB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
  B
B
B
B
B
B
B
B
B
B
B
B
B
B
%B
%B
%B
+B
+B
+B
	7B
	7B
	7B

=B
DB
DB
JB
JB
JB
JB
JB
DB

=B
	7B
	7B
	7B
	7B

=B
DB
DB
DB
JB
JB
JB
JB
JB
JB
PB
\B
bB
bB
oB
oB
uB
{B
{B
uB
{B
{B
{B
{B
{B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
 �B
!�B
!�B
�B
�B
 �B
 �B
 �B
!�B
!�B
"�B
!�B
 �B
"�B
+B
.B
7LB
;dB
C�B
H�B
N�B
T�B
[#B
`BB
dZB
iyB
n�B
r�B
v�B
z�B
~�B
�B
�%B
�D1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  B	u�B	u�B	t�B	u�B	t�B	u�B	t�B	t�B	t�B	t�B	u�B	u�B	t�B	t�B	t�B	t�B	t�B	t�B	t�B	t�B	t�B	t�B	u�B	w�B	~�B	�RB
 �B
{�B
�9B
��B
�B
B
��B
hsB
ZB
�+BB
�B
�wB
��B
�wB
��B
�B
�B
�B
��B
�jB
��B
�?B
��B
�HBBB
�B
��B
��B
��BBI�B<jBH�BD�B'�B�B
�B
ɺB
�wB
��B
|�B
bNB
N�B
1'B
�B
hB
'�B
%�B
u�B
�B
~�B
[#B
7LB
�B	ÖB	��B	�B	W
B	J�B	2-B	%�B	#�B	JB�B�5B�BǮB��B�jB�3B�B�3B�FB�FB�B�B��B�B��BɺB�fB�B	DB	&�B	J�B	hsB	}�B	l�B	O�B	<jB	33B	9XB	=qB	A�B	W
B	ZB	YB	YB	Q�B	?}B	0!B	VB�B�B�yB��B	�B	$�B	.B	#�B	uB	%B��B�B�B�B�B�B�B�B�B�B�B��B	�B	K�B	XB	z�B	|�B	y�B	w�B	dZB	P�B	8RB	�B	�B	#�B	 �B	&�B	'�B	+B	-B	2-B	7LB	5?B	49B	33B	1'B	49B	9XB	>wB	<jB	=qB	?}B	?}B	A�B	A�B	E�B	I�B	L�B	M�B	N�B	O�B	Q�B	S�B	XB	\)B	cTB	l�B	l�B	n�B	p�B	v�B	|�B	�B	�%B	�DB	~�B	}�B	~�B	}�B	z�B	|�B	~�B	� B	�B	�B	�B	�B	�B	�B	�B	�B	�1B	�1B	�=B	�JB	�JB	�JB	�PB	�hB	�oB	�uB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�B	��B	�B	�B	�B	�'B	�LB	�FB	�?B	�FB	�9B	�-B	�-B	�'B	�B	�B	�B	�B	�B	�B	�!B	�!B	�'B	�?B	�FB	�XB	�dB	�qB	�jB	�wB	�wB	�qB	�wB	�}B	�}B	B	ĜB	ŢB	ȴB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�
B	�#B	�#B	�#B	�/B	�5B	�NB	�`B	�`B	�TB	�fB	�yB	�yB	�sB	�yB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
  B
B
B
B
B
B
B
B
B
B
%B
%B
%B
%B
%B
+B
+B
+B
1B
1B
1B

=B

=B

=B
DB
JB
JB
PB
PB
PB
PB
PB
JB
DB

=B

=B

=B

=B
DB
JB
JB
DB
JB
JB
JB
JB
PB
PB
VB
\B
bB
bB
oB
oB
{B
�B
�B
{B
{B
�B
�B
�B
{B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
 �B
"�B
"�B
 �B
�B
 �B
 �B
 �B
!�B
!�B
#�B
"�B
 �B
#�B
+B
.B
7LB
;dB
D�B
I�B
O�B
VB
[#B
`BB
e`B
iyB
n�B
r�B
v�B
z�B
~�B
�B
�%B
�D1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  <#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
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
<e`B<D��<#�
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
<e`B<#�
<#�
<e`B<#�
<D��<#�
<#�
<e`B<e`B<#�
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
<u<D��<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
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
PRES            TEMP            PSAL            PRES_ADJUSTED = PRES - dP                                                                                                                                                                                                                                       none                                                                                                                                                                                                                                                            PSAL_ADJUSTED = sw_salt( sw_cndr(PSAL,TEMP,PRES), TEMP, PRES_ADJUSTED ) + CTM                                                                                                                                                                                   dP =0.4 dbar.                                                                                                                                                                                                                                                   none                                                                                                                                                                                                                                                            CTM: alpha=0.141, tau=6.68s                                                                                                                                                                                                                                     Pressures adjusted by using pressure offset at the sea surface. The quoted error is manufacturer specified accuracy in dbar.                                                                                                                                    The quoted error is manufacturer specified accuracy with respect to ITS-90 at time of laboratory calibration.                                                                                                                                                   No significant salinity drift detected. Salinity adjusted for effects of pressure adjustment. Conductivity cell thermal mass correction (CTM) applied to measurements taken in Cp mode. The quoted error is max[0.01, sqrt(OWerr^2+CTMerr^2)] in PSS-78.        201201131250112012011312501120120113125011  AO  ARGQ                                                                        20111205112511  QCP$                G�O�G�O�G�O�FFBFE           AO  ARGQ                                                                        20111205112511  QCF$                G�O�G�O�G�O�0               UW  ARSQUWQC    WOD & nearby Argo as visual check                               20120113125011  IP                  G�O�G�O�G�O�                