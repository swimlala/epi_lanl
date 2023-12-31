CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       N2014-12-25T13:31:08Z AOML 3.0 creation; 2016-06-01T00:08:22Z UW 3.1 conversion     
references        (http://www.argodatamgt.org/Documentation   comment       	free text      user_manual_version       3.1    Conventions       Argo-3.1 CF-1.6    featureType       trajectoryProfile         @   	DATA_TYPE                  	long_name         	Data type      conventions       Argo reference table 1     
_FillValue                    6�   FORMAT_VERSION                 	long_name         File format version    
_FillValue                    6�   HANDBOOK_VERSION               	long_name         Data handbook version      
_FillValue                    6�   REFERENCE_DATE_TIME                 	long_name         !Date of reference for Julian days      conventions       YYYYMMDDHHMISS     
_FillValue                    6�   DATE_CREATION                   	long_name         Date of file creation      conventions       YYYYMMDDHHMISS     
_FillValue                    6�   DATE_UPDATE                 	long_name         Date of update of this file    conventions       YYYYMMDDHHMISS     
_FillValue                    7    PLATFORM_NUMBER                   	long_name         Float unique identifier    conventions       WMO float identifier : A9IIIII     
_FillValue                    7   PROJECT_NAME                  	long_name         Name of the project    
_FillValue                  @  7   PI_NAME                   	long_name         "Name of the principal investigator     
_FillValue                  @  7X   STATION_PARAMETERS           	            	long_name         ,List of available parameters for the station   conventions       Argo reference table 3     
_FillValue                  0  7�   CYCLE_NUMBER               	long_name         Float cycle number     conventions       =0...N, 0 : launch cycle (if exists), 1 : first complete cycle      
_FillValue         ��        7�   	DIRECTION                  	long_name         !Direction of the station profiles      conventions       -A: ascending profiles, D: descending profiles      
_FillValue                    7�   DATA_CENTRE                   	long_name         .Data centre in charge of float data processing     conventions       Argo reference table 4     
_FillValue                    7�   DC_REFERENCE                  	long_name         (Station unique identifier in data centre   conventions       Data centre convention     
_FillValue                     7�   DATA_STATE_INDICATOR                  	long_name         1Degree of processing the data have passed through      conventions       Argo reference table 6     
_FillValue                    7�   	DATA_MODE                  	long_name         Delayed mode or real time data     conventions       >R : real time; D : delayed mode; A : real time with adjustment     
_FillValue                    7�   PLATFORM_TYPE                     	long_name         Type of float      conventions       Argo reference table 23    
_FillValue                     7�   FLOAT_SERIAL_NO                   	long_name         Serial number of the float     
_FillValue                     8   FIRMWARE_VERSION                  	long_name         Instrument firmware version    
_FillValue                     8<   WMO_INST_TYPE                     	long_name         Coded instrument type      conventions       Argo reference table 8     
_FillValue                    8\   JULD               	long_name         ?Julian day (UTC) of the station relative to REFERENCE_DATE_TIME    standard_name         time   units         "days since 1950-01-01 00:00:00 UTC     conventions       8Relative julian days with decimal part (as parts of day)   
resolution        >Ey��0�:   
_FillValue        A.�~       axis      T           8`   JULD_QC                	long_name         Quality on date and time   conventions       Argo reference table 2     
_FillValue                    8h   JULD_LOCATION                  	long_name         @Julian day (UTC) of the location relative to REFERENCE_DATE_TIME   units         "days since 1950-01-01 00:00:00 UTC     conventions       8Relative julian days with decimal part (as parts of day)   
resolution        >Ey��0�:   
_FillValue        A.�~            8l   LATITUDE               	long_name         &Latitude of the station, best estimate     standard_name         latitude   units         degree_north   
_FillValue        @�i�       	valid_min         �V�        	valid_max         @V�        axis      Y           8t   	LONGITUDE                  	long_name         'Longitude of the station, best estimate    standard_name         	longitude      units         degree_east    
_FillValue        @�i�       	valid_min         �f�        	valid_max         @f�        axis      X           8|   POSITION_QC                	long_name         ,Quality on position (latitude and longitude)   conventions       Argo reference table 2     
_FillValue                    8�   POSITIONING_SYSTEM                    	long_name         Positioning system     
_FillValue                    8�   VERTICAL_SAMPLING_SCHEME                  	long_name         Vertical sampling scheme   conventions       Argo reference table 16    
_FillValue                    8�   CONFIG_MISSION_NUMBER                  	long_name         :Unique number denoting the missions performed by the float     conventions       !1...N, 1 : first complete mission      
_FillValue         ��        9�   PROFILE_PRES_QC                	long_name         #Global quality flag of PRES profile    conventions       Argo reference table 2a    
_FillValue                    9�   PROFILE_TEMP_QC                	long_name         #Global quality flag of TEMP profile    conventions       Argo reference table 2a    
_FillValue                    9�   PROFILE_PSAL_QC                	long_name         #Global quality flag of PSAL profile    conventions       Argo reference table 2a    
_FillValue                    9�   PRES         
      
   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���   axis      Z        �  9�   PRES_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  A�   PRES_ADJUSTED            
      	   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  C�   PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  K|   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         decibar    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  Mx   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  Uh   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ]X   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  _T   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  gD   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         degree_Celsius     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  i@   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  q0   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  y    PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  {   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  �   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  0  ��   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                    �(   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                    �(   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                    �(   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  ,  �(   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                    �T   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                    �X   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                    �\   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                    �`   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  @  �d   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                    ��   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                    ��   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    ��   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar         ��   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar         ��   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�        ��   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    ��Argo profile    3.1 1.2 19500101000000  20141225133108  20160531170822  5903740 US ARGO PROJECT                                                 STEPHEN RISER                                                   PRES            TEMP            PSAL               eA   AO  4055_7112_101                   2C  D   APEX                            5374                            041511                          846 @�-�' 1   @�-��Р
@9��vȴ9�dA�+1   GPS     Primary sampling: mixed [deeper than nominal 985dbar: discrete; nominal 985dbar to surface: 2dbar-bin averaged]                                                                                                                                                    eA   A   A   @���@�  A   A   A@  A`  A�  A�  A�  A�  A�  A�  A�  A�  B   B  B  B  B   B(  B0  B8  B@  BH  BP  BX  B`  Bh  Bp  Bx  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�33B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  C   C  C  C  C  C
  C  C  C  C  C  C  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dc��Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dt` Dyl�D�#3D�I�D�L�D���D���D�\�D���D�� D�3D�@ D��3D��3D�fD�FfDڀ D�fD�  D�33D�` 1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111@�fg@���AffA&ffAFffAfffA�33A�33A�33A�33A�33A�33A�33A�33B��B	��B��B��B!��B)��B1��B9��BA��BI��BQ��BY��Ba��Bi��Bq��By��B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B�  B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���C ffCffCffCffCffC
ffCffCffCffCffCffCffCffCffCffCffC ffC"ffC$ffC&ffC(ffC*ffC,ffC.ffC0ffC2ffC4ffC6ffC8ffC:ffC<ffC>ffC@ffCBffCDffCFffCHffCJffCLffCNffCPffCRffCTffCVffCXffCZffC\ffC^ffC`ffCbffCdffCfffChffCjffClffCnffCpffCrffCtffCvffCxffCzffC|ffC~ffC�33C�33C�33C�33C�33C�33C�33C�33C�33C�33C�33C�33C�33C�33C�33C�33C�33C�33C�33C�33C�33C�33C�33C�33C�33C�33C�33C�33C�33C�33C�33C�33C�33C�33C�33C�33C�33C�33C�33C�33C�33C�33C�33C�33C�33C�33C�33C�33C�33C�33C�33C�33C�33C�33C�33C�33C�33C�33C�33C�33C�33C�33C�33C�33C�33C�33C�33C�33C�33C�33C�33C�33C�33C�33C�33C�33C�33C�33C�33C�33C�33C�33C�33C�33C�33C�33C�33C�33C�33C�33C�33C�33C�33C�33C�33C�33C�33C�33C�33C�33C�33C�33C�33C�33C�33C�33C�33C�33C�33C�33C�33C�33C�33C�33C�33C�33C�33C�33C�33C�33C�33C�33C�33C�33C�33C�33C�33C�33D �D ��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D	�D	��D
�D
��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D �D ��D!�D!��D"�D"��D#�D#��D$�D$��D%�D%��D&�D&��D'�D'��D(�D(��D)�D)��D*�D*��D+�D+��D,�D,��D-�D-��D.�D.��D/�D/��D0�D0��D1�D1��D2�D2��D3�D3��D4�D4��D5�D5��D6�D6��D7�D7��D8�D8��D9�D9��D:�D:��D;�D;��D<�D<��D=�D=��D>�D>��D?�D?��D@�D@��DA�DA��DB�DB��DC�DC��DD�DD��DE�DE��DF�DF��DG�DG��DH�DH��DI�DI��DJ�DJ��DK�DK��DL�DL��DM�DM��DN�DN��DO�DO��DP�DP��DQ�DQ��DR�DR��DS�DS��DT�DT��DU�DU��DV�DV��DW�DW��DX�DX��DY�DY��DZ�DZ��D[�D[��D\�D\��D]�D]��D^�D^��D_�D_��D`�D`��Da�Da��Db�Db��Dc�Dc��Dd4Dd��De�De��Df�Df��Dg�Dg��Dh�Dh��Di�Di��Dj�Dj��Dk�Dk��Dl�Dl��Dm�Dm��Dn�Dn��Do�Do��Dp�Dp��Dq�Dq��Dr�Dr��Ds�Ds��Dt�Dty�Dy�gD�0 D�VgD�Y�D��gD�	�D�i�D��gD���D� D�L�D�� D�� D�3D�S3Dڌ�D��3D��D�@ D�l�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��A�ƨA�ȴA�A���A���A�ĜA���A���A���A��A��A��
A��A��#A��;A��;A��;A��/A��`A��TA��yA��yA��mA��yA��yA��yA��/A��;A��#A��;A��A���A���A���A���A���A�ƨA���A���A�XA�n�A���A�"�A���A�l�A�-A���A���A�A���A�&�A�A�S�A��A�&�A��#A���A�\)A�A���A�p�A��TA�l�A��FA�1A�{A���A�`BA��A�"�A���A�^5A�dZA�9XA��-A�ffA��7A~�yA~-A}��A{t�Ayl�Ax1Avr�Au��As��Ar{Ap�yAn��Am�-Am/Al1'Aj5?Ah�Af��Ae��Ad�`AcAb��AbZA`��A`(�A_��A_x�A_+A^ZA\�HA\bA[AZ�jAZM�AZ$�AYƨAX�`AXz�AX�AW�AVr�AU�7AT��ASAS33AR�/AQC�APZAO`BANbNAM�AL��AK�AJ5?AH�AG`BAFE�AFAE��AEl�AEK�AD��AD�+AC��AChsABr�AA��A@ZA>^5A=S�A:  A9�A8�/A8{A7�PA7VA6z�A5��A4�A3�A2�RA2^5A25?A25?A2$�A2�A1��A1�TA1A1?}A0ĜA0(�A0bA/�wA/��A/hsA.��A-��A,^5A+%A*~�A*�A'
=A%A%XA%�A$�A$��A$^5A$JA#�^A#C�A"�`A"ZA!��A �+A�A�yAE�AdZAȴAjA��A�9A�^A?}A�AZA;dAffA/A~�Ax�A��AE�AdZA��AXA��Ap�A	�A	t�A	K�A	�A��A�A�A��A�\A7LAI�AS�A $�@�+@��T@�J@�Ĝ@��w@��H@��+@���@�O�@���@�(�@�dZ@�7L@�(�@�n�@���@�Z@�J@�dZ@���@��@�@㝲@��H@�v�@�hs@�I�@��;@�"�@�-@��y@�V@���@�b@���@�Ĝ@�ƨ@�;d@�^5@�z�@�+@��#@�bN@�
=@�M�@�hs@���@���@���@�l�@���@�^5@���@�&�@���@���@�+@�ff@�&�@�Q�@�\)@�ff@��#@�X@�Ĝ@�b@���@���@�x�@�O�@�&�@��@��@��F@�M�@��@�ƨ@���@�ff@�-@��@�&�@���@��9@�I�@��
@���@�-@��7@���@��D@�r�@��w@�"�@��@��y@�V@�J@���@��F@�"�@��R@�~�@�E�@�{@���@��7@�G�@��@��@��D@�Q�@�1@��w@�"�@���@��@��`@��;@���@�V@���@�(�@���@��^@��7@�O�@�%@��D@���@��@��\@�5?@��T@��@�/@�%@���@���@�Ĝ@���@��D@�r�@�(�@�  @��m@��;@�ƨ@�|�@�33@�@��@�~�@��@��T@��^@�p�@�?}@���@���@���@�bN@�1'@�b@��@�|�@�l�@�;d@�
=@��y@���@�^5@�V@�-@��@�@��7@�&�@�Ĝ@��u@��u@��D@��D@��@�j@�Z@�A�@���@��;@���@�t�@�l�@�;d@��!@���@�o@�"�@���@���@�5?@��^@���@��@�%@���@���@��@�Z@�1@�w@��@�P@l�@~��@~�@~ȴ@~��@~V@}@|��@|�D@|1@{�@{"�@z��@y�^@yx�@yx�@yhs@yG�@x�`@x�u@x �@wl�@w;d@vE�@u�-@u/@t�j@tz�@s�
@s�@st�@st�@st�@sdZ@s33@r��@rn�@r-@q��@qx�@q�@p��@p��@pA�@p  @o�@k"�@a�^@W�P@R��@L�@G;d@A��@;�@7;d@2n�@,��@$9X@O�@��@{@��@  @O�@	%@v�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111A�ƨA�ȴA�A���A���A�ĜA���A���A���A��A��A��
A��A��#A��;A��;A��;A��/A��`A��TA��yA��yA��mA��yA��yA��yA��/A��;A��#A��;A��A���A���A���A���A���A�ƨA���A���A�XA�n�A���A�"�A���A�l�A�-A���A���A�A���A�&�A�A�S�A��A�&�A��#A���A�\)A�A���A�p�A��TA�l�A��FA�1A�{A���A�`BA��A�"�A���A�^5A�dZA�9XA��-A�ffA��7A~�yA~-A}��A{t�Ayl�Ax1Avr�Au��As��Ar{Ap�yAn��Am�-Am/Al1'Aj5?Ah�Af��Ae��Ad�`AcAb��AbZA`��A`(�A_��A_x�A_+A^ZA\�HA\bA[AZ�jAZM�AZ$�AYƨAX�`AXz�AX�AW�AVr�AU�7AT��ASAS33AR�/AQC�APZAO`BANbNAM�AL��AK�AJ5?AH�AG`BAFE�AFAE��AEl�AEK�AD��AD�+AC��AChsABr�AA��A@ZA>^5A=S�A:  A9�A8�/A8{A7�PA7VA6z�A5��A4�A3�A2�RA2^5A25?A25?A2$�A2�A1��A1�TA1A1?}A0ĜA0(�A0bA/�wA/��A/hsA.��A-��A,^5A+%A*~�A*�A'
=A%A%XA%�A$�A$��A$^5A$JA#�^A#C�A"�`A"ZA!��A �+A�A�yAE�AdZAȴAjA��A�9A�^A?}A�AZA;dAffA/A~�Ax�A��AE�AdZA��AXA��Ap�A	�A	t�A	K�A	�A��A�A�A��A�\A7LAI�AS�A $�@�+@��T@�J@�Ĝ@��w@��H@��+@���@�O�@���@�(�@�dZ@�7L@�(�@�n�@���@�Z@�J@�dZ@���@��@�@㝲@��H@�v�@�hs@�I�@��;@�"�@�-@��y@�V@���@�b@���@�Ĝ@�ƨ@�;d@�^5@�z�@�+@��#@�bN@�
=@�M�@�hs@���@���@���@�l�@���@�^5@���@�&�@���@���@�+@�ff@�&�@�Q�@�\)@�ff@��#@�X@�Ĝ@�b@���@���@�x�@�O�@�&�@��@��@��F@�M�@��@�ƨ@���@�ff@�-@��@�&�@���@��9@�I�@��
@���@�-@��7@���@��D@�r�@��w@�"�@��@��y@�V@�J@���@��F@�"�@��R@�~�@�E�@�{@���@��7@�G�@��@��@��D@�Q�@�1@��w@�"�@���@��@��`@��;@���@�V@���@�(�@���@��^@��7@�O�@�%@��D@���@��@��\@�5?@��T@��@�/@�%@���@���@�Ĝ@���@��D@�r�@�(�@�  @��m@��;@�ƨ@�|�@�33@�@��@�~�@��@��T@��^@�p�@�?}@���@���@���@�bN@�1'@�b@��@�|�@�l�@�;d@�
=@��y@���@�^5@�V@�-@��@�@��7@�&�@�Ĝ@��u@��u@��D@��D@��@�j@�Z@�A�@���@��;@���@�t�@�l�@�;d@��!@���@�o@�"�@���@���@�5?@��^@���@��@�%@���@���@��@�Z@�1@�w@��@�P@l�@~��@~�@~ȴ@~��@~V@}@|��@|�D@|1@{�@{"�@z��@y�^@yx�@yx�@yhs@yG�@x�`@x�u@x �@wl�@w;d@vE�@u�-@u/@t�j@tz�@s�
@s�@st�@st�@st�@sdZ@s33@r��@rn�@r-@q��@qx�@q�@p��@p��@pA�@p  @o�@k"�@a�^@W�P@R��@L�@G;d@A��@;�@7;d@2n�@,��@$9X@O�@��@{@��@  @O�@	%@v�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oBP�BP�BP�BP�BQ�BQ�BP�BP�BQ�BQ�BP�BP�BP�BP�BP�BP�BP�BP�BQ�BQ�BQ�BQ�BQ�BQ�BQ�BQ�BO�BP�BO�BO�BO�BN�BN�BN�BM�BM�BL�BH�BF�B>wB&�B{B��B��BVB�mB�}B��B�Br�BiyBR�B0!B{B  B��B��B�B�B�#B��BǮB�}B�!B��B�VBt�B\)B=qB�B
=B
�HB
�'B
��B
��B
�VB
z�B
s�B
n�B
jB
ZB
J�B
<jB
-B
&�B
�B
bB
	7B	��B	�B	�B	�sB	�5B	��B	��B	ǮB	B	�}B	�XB	�FB	�B	�B	��B	��B	��B	��B	��B	��B	�uB	�oB	�hB	�\B	�PB	�=B	�1B	�B	�B	{�B	v�B	r�B	m�B	iyB	ffB	`BB	\)B	W
B	P�B	J�B	F�B	A�B	=qB	8RB	/B	'�B	&�B	$�B	#�B	"�B	 �B	�B	�B	�B	oB	VB	1B��B�B�#B��B��B��B��BɺBǮBĜB��B�wB�jB�dB�dB�dB�^B�^B�XB�XB�RB�FB�?B�FB�FB�?B�?B�9B�-B�B�B��B��B��B��B��B��B�{B�uB�oB�hB�bB�\B�VB�JB�7B�%B�B~�B{�By�Bw�Bu�Bs�Bp�Bm�BjBgmBbNB^5BZBW
BR�BP�BM�BJ�BH�BE�BB�B?}B<jB9XB6FB5?B33B0!B,B)�B(�B'�B%�B"�B�B�B�B�B�BuBoBhBhBbBbB\BVBVBJBDB	7B	7B1B%BBB%BBBBBBBBBB��B  B��B��BBBBBBBBB%B+B	7B	7B
=B	7BJB\BoBoBuB{B{B�B�B�B�B�B�B�B�B!�B!�B"�B#�B'�B)�B+B,B,B,B,B.B1'B5?B9XB<jB=qB>wB@�BB�BB�BB�BC�BD�BI�BL�BO�BR�BS�BS�BW
BYBYBYB[#B[#B`BBdZBgmBiyBjBk�Bk�Bm�Bn�Bo�Bp�Br�Br�Bs�Bt�Bt�Bw�Bx�B|�B�B�%B�VB�VB��B��B��B�B�B�B�'B�9B�RB�}BĜBǮBɺB��B��B��B��B��B��B�B�
B�
B�B�)B�/B�/B�/B�BB�TB�`B�fB�yB�B�B�B�B��B��B��B��B��B��B	  B	B	1B		7B	DB	PB	VB	bB	uB	{B	�B	�B	�B	�B	#�B	,B	1'B	2-B	2-B	33B	49B	7LB	8RB	9XB	>wB	A�B	C�B	D�B	D�B	E�B	L�B	O�B	S�B	VB	XB	[#B	_;B	bNB	dZB	gmB	jB	l�B	n�B	p�B	s�B	w�B	z�B	z�B	{�B	{�B	~�B	� B	� B	�B	�B	�B	�1B	�=B	�JB	�\B	�bB	�oB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�B	�B	�B	�!B	�!B	�'B	�'B	�'B	�'B	�-B	�9B	B	�B	��B	��B
PB
�B
 �B
+B
2-B
9XB
@�B
K�B
S�B
XB
]/B
aHB
e`B
hsB
n�B
r�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111BP�BP�BP�BP�BQ�BQ�BP�BP�BQ�BQ�BP�BP�BP�BP�BP�BP�BP�BP�BQ�BQ�BQ�BQ�BQ�BQ�BQ�BQ�BO�BP�BO�BO�BO�BN�BN�BN�BM�BM�BL�BH�BF�B>^B&�B`B��B��B0B�HB�[B��B��Br�BiUBR�B/�BUB��B��B��B�B�ZB��B��BǉB�WB��B��B�1Bt�B\B=LByB
B
�#B
�B
��B
�bB
�1B
z�B
s�B
nsB
jZB
Y�B
J�B
<GB
,�B
&�B
wB
AB
	B	��B	�B	�uB	�RB	�B	��B	ͰB	ǏB	�lB	�\B	�6B	�%B	��B	��B	��B	��B	��B	�sB	�nB	�`B	�VB	�PB	�HB	�<B	�3B	�B	�B	��B	��B	{�B	v�B	r�B	mtB	iZB	fGB	`$B	\
B	V�B	P�B	J�B	F�B	AmB	=UB	87B	.�B	'�B	&�B	$�B	#�B	"�B	 �B	�B	xB	jB	SB	<B	B��B�B�B��B��B̴BˮBɣBǕBąB�iB�]B�OB�JB�LB�MB�DB�EB�>B�>B�7B�-B�'B�/B�,B�&B�(B�B�B��B��B��B��B��B��B�oB�iB�cB�]B�XB�PB�JB�DB�=B�1B�B�B��B~�B{�By�Bw�Bu�Bs�Bp�BmwBjgBgVBb9B^ BZBV�BR�BP�BM�BJ�BH�BE�BByB?eB<RB9DB62B5*B3B0B+�B)�B(�B'�B%�B"�B�B�BvBhBVBDB?B6B7BLB2BEB&B$BB.B	B	 B B�B�B�B�B�B�BB�B�B�B�B�B �B��B��B��B��B �B�B�B�B�B�BB�B�B�B	 B	B
%B	B3BCB;BVB[BcBGBsBuBtBzB�BqB�B�B!�B!�B"�B#�B'�B)�B*�B+�B+�B+�B+�B-�B1B5$B9<B<LB=UB>XB@fBBsBBpBBoBCyBD�BI�BL�BO�BR�BS�BS�BV�BX�BX�BX�B[B[B`#Bd:BgPBiWBj_BkfBkeBmqBnyBo�Bp�Br�Br�Bs�Bt�Bt�Bw�Bx�B|�B��B�B�4B�7B�eB�B��B��B��B��B�B�B�.B�YB�yBǊBɖBͮB��B��B��B��B��B��B��B��B��B�B�	B�	B�
B�B�1B�7B�BB�VB�fB�qB�yB�B��B��B��B��B��B��B��B	�B	
B		B	B	(B	0B	;B	MB	TB	WB	lB	zB	�B	#�B	+�B	0�B	2B	2B	3B	4B	7#B	8*B	90B	>NB	A`B	CnB	DsB	DsB	ExB	L�B	O�B	S�B	U�B	W�B	Z�B	_B	b B	d1B	gBB	jRB	laB	npB	pxB	s�B	w�B	z�B	z�B	{�B	{�B	~�B	�B	�B	��B	��B	��B	�
B	�B	�B	�0B	�8B	�DB	�\B	�cB	�cB	�gB	�gB	�mB	�uB	�zB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	� B	�B	�dB	��B	�B	��B
B
XB
 �B
*�B
2 B
9)B
@RB
K�B
S�B
W�B
\�B
aB
e0B
hCB
neB
r�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
PRES            TEMP            PSAL            PRES_ADJUSTED = PRES - dP                                                                                                                                                                                                                                       none                                                                                                                                                                                                                                                            PSAL_ADJUSTED = sw_salt( sw_cndr(PSAL,TEMP,PRES), TEMP, PRES_ADJUSTED )                                                                                                                                                                                         dP =-0.4 dbar.                                                                                                                                                                                                                                                  none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            Pressures adjusted by using pressure offset at the sea surface. The quoted error is manufacturer specified accuracy in dbar.                                                                                                                                    The quoted error is manufacturer specified accuracy with respect to ITS-90 at time of laboratory calibration.                                                                                                                                                   No significant salinity drift detected. Salinity adjusted for effects of pressure adjustment. The quoted error is max[0.01, 1xOW uncertainty] in PSS-78.                                                                                                        201605311708222016053117082220160531170822  AO  ARCAADJP                                                                    20141225133108    IP                G�O�G�O�G�O�                AO  ARGQQCPL                                                                    20141225133108  QCP$                G�O�G�O�G�O�DFB7E           AO  ARGQQCPL                                                                    20141225133108  QCF$                G�O�G�O�G�O�0               UW  ARSQUWQC    WOD & nearby Argo as visual check                               20160531170822  IP                  G�O�G�O�G�O�                