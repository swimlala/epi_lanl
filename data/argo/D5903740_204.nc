CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             
   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       2017-11-15T18:01:37Z creation      
references        (http://www.argodatamgt.org/Documentation   comment       	free text      user_manual_version       3.1    Conventions       Argo-3.1 CF-1.6    featureType       trajectoryProfile      comment_dmqc_operator         (Matthew Alkire, University of Washington      @   	DATA_TYPE                  	long_name         	Data type      conventions       Argo reference table 1     
_FillValue                    6�   FORMAT_VERSION                 	long_name         File format version    
_FillValue                    6�   HANDBOOK_VERSION               	long_name         Data handbook version      
_FillValue                    6�   REFERENCE_DATE_TIME                 	long_name         !Date of reference for Julian days      conventions       YYYYMMDDHHMISS     
_FillValue                    6�   DATE_CREATION                   	long_name         Date of file creation      conventions       YYYYMMDDHHMISS     
_FillValue                    7   DATE_UPDATE                 	long_name         Date of update of this file    conventions       YYYYMMDDHHMISS     
_FillValue                    7   PLATFORM_NUMBER                   	long_name         Float unique identifier    conventions       WMO float identifier : A9IIIII     
_FillValue                    7,   PROJECT_NAME                  	long_name         Name of the project    
_FillValue                  @  74   PI_NAME                   	long_name         "Name of the principal investigator     
_FillValue                  @  7t   STATION_PARAMETERS           	            	long_name         ,List of available parameters for the station   conventions       Argo reference table 3     
_FillValue                  0  7�   CYCLE_NUMBER               	long_name         Float cycle number     conventions       =0...N, 0 : launch cycle (if exists), 1 : first complete cycle      
_FillValue         ��        7�   	DIRECTION                  	long_name         !Direction of the station profiles      conventions       -A: ascending profiles, D: descending profiles      
_FillValue                    7�   DATA_CENTRE                   	long_name         .Data centre in charge of float data processing     conventions       Argo reference table 4     
_FillValue                    7�   DC_REFERENCE                  	long_name         (Station unique identifier in data centre   conventions       Data centre convention     
_FillValue                     7�   DATA_STATE_INDICATOR                  	long_name         1Degree of processing the data have passed through      conventions       Argo reference table 6     
_FillValue                    8   	DATA_MODE                  	long_name         Delayed mode or real time data     conventions       >R : real time; D : delayed mode; A : real time with adjustment     
_FillValue                    8   PLATFORM_TYPE                     	long_name         Type of float      conventions       Argo reference table 23    
_FillValue                     8   FLOAT_SERIAL_NO                   	long_name         Serial number of the float     
_FillValue                     88   FIRMWARE_VERSION                  	long_name         Instrument firmware version    
_FillValue                     8X   WMO_INST_TYPE                     	long_name         Coded instrument type      conventions       Argo reference table 8     
_FillValue                    8x   JULD               	long_name         ?Julian day (UTC) of the station relative to REFERENCE_DATE_TIME    standard_name         time   units         "days since 1950-01-01 00:00:00 UTC     conventions       8Relative julian days with decimal part (as parts of day)   
resolution        >�EȠ      
_FillValue        A.�~       axis      T           8|   JULD_QC                	long_name         Quality on date and time   conventions       Argo reference table 2     
_FillValue                    8�   JULD_LOCATION                  	long_name         @Julian day (UTC) of the location relative to REFERENCE_DATE_TIME   units         "days since 1950-01-01 00:00:00 UTC     conventions       8Relative julian days with decimal part (as parts of day)   
resolution        >�EȠ      
_FillValue        A.�~            8�   LATITUDE               	long_name         &Latitude of the station, best estimate     standard_name         latitude   units         degree_north   
_FillValue        @�i�       	valid_min         �V�        	valid_max         @V�        axis      Y           8�   	LONGITUDE                  	long_name         'Longitude of the station, best estimate    standard_name         	longitude      units         degree_east    
_FillValue        @�i�       	valid_min         �f�        	valid_max         @f�        axis      X           8�   POSITION_QC                	long_name         ,Quality on position (latitude and longitude)   conventions       Argo reference table 2     
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
_FillValue                    A�   PRES_ADJUSTED            
      	   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  C�   PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                    K�   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         decibar    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  M�   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  U�   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                    ]�   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  _�   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                    g�   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         degree_Celsius     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  i�   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  q�   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                    y�   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  {�   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                    �|   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  �|   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  0  �t   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                    ��   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                    ��   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                    ��   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  ,  ��   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                    ��   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                    ��   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                    ��   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                    ��   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  @  ��   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                    �    HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                    �0   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    �4   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar         �D   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar         �H   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�        �L   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    �PArgo profile    3.1 1.2 19500101000000  20171115180137  20190604095309  5903740 US ARGO PROJECT                                                 STEPHEN RISER                                                   PRES            TEMP            PSAL               �A   AO  4055                            2C  D   APEX                            5374                            041511                          846 @�5����1   @�5�m	�\@:{��S���c<j~��#1   GPS     Primary sampling: mixed [deeper than nominal 985dbar: discrete; nominal 985dbar to surface: 2dbar-bin averaged]                                                                                                                                                    �A   B   B   @�33@�33A   A!��A<��A^ffA�  A�  A�  A�  A�  A�  A�  A�  B   B  B  B  B   B(  B0  B8  B@  BH  BP  BX  B`  Bh  Bp  Bx  B�  B�  B�  B�  B�  B�33B�33B���B���B���B�  B�  B�  B�  B�  B�33B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  C   C  C  C  C  C
  C  C  C  C  C  C  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dt� Dt��Dyy�D�	HD�A�D�}D��)D�RD�R�D���D���D�	�D�/�D���D���D��D�EDڊ�D��D��D��D� D�uq111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  @�  @�  AffA(  AC33Ad��A�33A�33A�33A�33A�33A�33A�33A�33B��B	��B��B��B!��B)��B1��B9��BA��BI��BQ��BY��Ba��Bi��Bq��By��B���B���B���B���B���B�  B�  B�fgB���B���B���B���B���B���B���B�  B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���C ffCffCffCffCffC
ffCffCffCffCffCffCffCffCffCffCffC ffC"ffC$ffC&ffC(ffC*ffC,ffC.ffC0ffC2ffC4ffC6ffC8ffC:ffC<ffC>ffC@ffCBffCDffCFffCHffCJffCLffCNffCPffCRffCTffCVffCXffCZffC\ffC^ffC`ffCbffCdffCfffChffCjffClffCnffCpffCrffCtffCvffCxffCzffC|ffC~ffC�33C�33C�33C�33C�33C�33C�33C�33C�33C�33C�33C�33C�33C�33C�33C�33C�33C�33C�33C�33C�33C�33C�33C�33C�33C�33C�33C�33C�33C�33C�33C�33C�33C�33C�33C�33C�33C�33C�33C�33C�33C�33C�33C�33C�33C�33C�@ C�@ C�33C�33C�33C�33C�33C�33C�33C�33C�33C�33C�33C�33C�33C�33C�33C�33C�33C�33C�33C�33C�33C�33C�33C�33C�33C�33C�33C�33C�33C�33C�33C�33C�33C�33C�33C�33C�33C�33C�33C�33C�33C�33C�33C�33C�33C�33C�33C�33C�33C�33C�33C�33C�33C�33C�33C�33C�33C�33C�33C�33C�33C�33C�33C�33C�33C�33C�33C�33C�33C�33C�33C�33C�33C�33C�33C�33C�33C�33C�33C�33D �D ��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D	�D	��D
�D
��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D �D ��D!�D!��D"�D"��D#�D#��D$�D$��D%�D%��D&�D&��D'�D'��D(�D(��D)�D)��D*�D*��D+�D+��D,�D,��D-�D-��D.�D.��D/�D/��D0�D0��D1�D1��D2�D2��D3�D3��D4�D4��D5�D5��D6�D6��D7�D7��D8�D8��D9�D9��D:�D:��D;�D;��D<�D<��D=�D=��D>�D>��D?�D?��D@�D@��DA�DA��DB�DB��DC�DC��DD�DD��DE�DE��DF�DF��DG�DG��DH�DH��DI�DI��DJ�DJ��DK�DK��DL�DL��DM�DM��DN�DN��DO�DO��DP�DP��DQ�DQ��DR�DR��DS�DS��DT�DT��DU�DU��DV�DV��DW�DW��DX�DX��DY�DY��DZ�DZ��D[�D[��D\�D\��D]�D]��D^�D^��D_�D_��D`�D`��Da�Da��Db�Db��Dc�Dc��Dd�Dd��De�De��Df�Df��Dg�Dg��Dh�Dh��Di�Di��Dj�Dj��Dk�Dk��Dl�Dl��Dm�Dm��Dn�Dn��Do�Do��Dp�Dp��Dq�Dq��Dr�Dr��Ds�Ds��Dt�Dt��Dt�gDy�4D�D�N�D���D���D�D�_�D���D��qD��D�<{D��{D��D��D�Q�Dڗ\D�ɚD��qD�+�D��D��>111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  @��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��A̾wA�A̾wA̾wA̺^A���A�ƨA�ƨA���A�ĜA�A�A���A��A��#A��#A��A��;A��;A��/A��HA��TA��`A��yA��TA��A���A�%A�VA�{A��A�$�A�-A�-A�+A̬A�ƨA��A�bNA�`BA��`A���A�oA�1'A��A���A��A���A�\)A��
A��A�bNA�bA���A�x�A�XA�\)A��7A���A�dZA�XA��7A�G�A���A���A�x�A��
A�+A���A��A��TA�?}A��TA��9A��A��A�t�A�bNA���A�1A�S�A�7LA�v�A��/A��TA�/A��7A�JA�\)A��PA��yA�VA�ȴA�E�A��A�z�A�r�A���A���A��A��mA��A�A��^A�1A��#A���A�|�A���A��A~~�A}
=Az��Ax�jAw
=As`BAo�
AmƨAjM�Ah�\Ag7LAe�TAd�AdM�Ac33Ab��Aa�Aa+A`��A_��A_C�A_G�A^z�A]S�A\A[l�AZ�RAYO�AW�;AWVAVn�AU��AS��ASx�AS��AR��AP(�AO�AN�!ANjAM�AL�uAK�AIG�AGAFM�AE�AD�AD(�AC
=AA`BA?O�A>�A=�TA<�A;A:��A9hsA8 �A7&�A6��A65?A4�jA3S�A2�A1�mA0v�A/C�A.��A.��A.�+A-ƨA,I�A*�9A*ffA*M�A*JA)�A(��A(�A'�-A'VA%�
A&�A&VA%"�A$��A$�A%VA$�jA#�^A"�!A"jA!��A ��A�#A&�Az�AhsAE�A�A+AJA|�A��Az�A�PA�A?}AA-A��AhsA�DAdZAVA��AC�A
M�A	
=AZA?}A��A1'A�A|�A33A�HA�+A��A7LA�/AQ�A��AK�A ��A ff@�|�@��7@�Q�@�l�@��7@�1'@���@��u@�M�@���@�v�@�7@�V@��@�{@�dZ@�E�@��@�n�@��@�I�@�ȴ@�M�@�  @���@���@ۍP@ڇ+@���@�  @���@��@�1'@�|�@��@���@Гu@�v�@��`@�j@��H@�V@�j@��m@�+@��@�`B@Ĵ9@���@��`@��F@���@�V@�|�@���@��-@�7L@�V@��@�j@��!@�5?@��#@��@���@��F@�33@��@��@�E�@�7L@���@��;@�33@��F@�S�@�v�@��h@���@�  @�ƨ@���@���@�r�@�ƨ@���@���@�-@��/@��@��@��
@�33@��y@�M�@�J@��#@�&�@���@�bN@� �@�I�@��w@���@��@��@��@�j@��
@�
=@��!@��+@��@�x�@���@�bN@�ƨ@��y@��@�/@��9@�Z@��P@��@��\@�5?@�@��7@��@��/@��9@� �@��F@�S�@���@�-@���@��^@��@��u@�A�@�ƨ@��+@��@��#@��7@�7L@���@��`@��/@���@�j@�  @�l�@��@���@�ȴ@�ȴ@���@�5?@�x�@�X@�&�@��@���@�I�@��m@���@��@�|�@�K�@�+@���@�ff@���@��h@�O�@�&�@���@��j@�bN@�Q�@�b@�  @�@|�@~�y@~E�@~@}�T@}@}�h@}`B@}O�@}?}@}?}@}/@}�@}�@}�@}V@}�@|�@|�D@|9X@{�
@{��@{dZ@{"�@z~�@z^5@z^5@z-@y�@y��@yhs@yG�@x��@x�9@x��@x��@xbN@x �@w�;@w�@w;d@w�@v�y@v�R@vV@v5?@v@u��@u�@u`B@t�@t(�@s�@r�@r��@rM�@r-@p1@i�@d�@]/@XD�@T7@M�N@F.�@?�Q@:~�@3)_@.�+@(��@"��@�@��@��@c @�@C@/�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111  A̾wA�A̾wA̾wA̺^A���A�ƨA�ƨA���A�ĜA�A�A���A��A��#A��#A��A��;A��;A��/A��HA��TA��`A��yA��TA��A���A�%A�VA�{A��A�$�A�-A�-A�+A̬A�ƨA��A�bNA�`BA��`A���A�oA�1'A��A���A��A���A�\)A��
A��A�bNA�bA���A�x�A�XA�\)A��7A���A�dZA�XA��7A�G�A���A���A�x�A��
A�+A���A��A��TA�?}A��TA��9A��A��A�t�A�bNA���A�1A�S�A�7LA�v�A��/A��TA�/A��7A�JA�\)A��PA��yA�VA�ȴA�E�A��A�z�A�r�A���A���A��A��mA��A�A��^A�1A��#A���A�|�A���A��A~~�A}
=Az��Ax�jAw
=As`BAo�
AmƨAjM�Ah�\Ag7LAe�TAd�AdM�Ac33Ab��Aa�Aa+A`��A_��A_C�A_G�A^z�A]S�A\A[l�AZ�RAYO�AW�;AWVAVn�AU��AS��ASx�AS��AR��AP(�AO�AN�!ANjAM�AL�uAK�AIG�AGAFM�AE�AD�AD(�AC
=AA`BA?O�A>�A=�TA<�A;A:��A9hsA8 �A7&�A6��A65?A4�jA3S�A2�A1�mA0v�A/C�A.��A.��A.�+A-ƨA,I�A*�9A*ffA*M�A*JA)�A(��A(�A'�-A'VA%�
A&�A&VA%"�A$��A$�A%VA$�jA#�^A"�!A"jA!��A ��A�#A&�Az�AhsAE�A�A+AJA|�A��Az�A�PA�A?}AA-A��AhsA�DAdZAVA��AC�A
M�A	
=AZA?}A��A1'A�A|�A33A�HA�+A��A7LA�/AQ�A��AK�A ��A ff@�|�@��7@�Q�@�l�@��7@�1'@���@��u@�M�@���@�v�@�7@�V@��@�{@�dZ@�E�@��@�n�@��@�I�@�ȴ@�M�@�  @���@���@ۍP@ڇ+@���@�  @���@��@�1'@�|�@��@���@Гu@�v�@��`@�j@��H@�V@�j@��m@�+@��@�`B@Ĵ9@���@��`@��F@���@�V@�|�@���@��-@�7L@�V@��@�j@��!@�5?@��#@��@���@��F@�33@��@��@�E�@�7L@���@��;@�33@��F@�S�@�v�@��h@���@�  @�ƨ@���@���@�r�@�ƨ@���@���@�-@��/@��@��@��
@�33@��y@�M�@�J@��#@�&�@���@�bN@� �@�I�@��w@���@��@��@��@�j@��
@�
=@��!@��+@��@�x�@���@�bN@�ƨ@��y@��@�/@��9@�Z@��P@��@��\@�5?@�@��7@��@��/@��9@� �@��F@�S�@���@�-@���@��^@��@��u@�A�@�ƨ@��+@��@��#@��7@�7L@���@��`@��/@���@�j@�  @�l�@��@���@�ȴ@�ȴ@���@�5?@�x�@�X@�&�@��@���@�I�@��m@���@��@�|�@�K�@�+@���@�ff@���@��h@�O�@�&�@���@��j@�bN@�Q�@�b@�  @�@|�@~�y@~E�@~@}�T@}@}�h@}`B@}O�@}?}@}?}@}/@}�@}�@}�@}V@}�@|�@|�D@|9X@{�
@{��@{dZ@{"�@z~�@z^5@z^5@z-@y�@y��@yhs@yG�@x��@x�9@x��@x��@xbN@x �@w�;@w�@w;d@w�@v�y@v�R@vV@v5?@v@u��@u�@u`B@t�@t(�@s�@r�@r��@rM�G�O�@p1@i�@d�@]/@XD�@T7@M�N@F.�@?�Q@:~�@3)_@.�+@(��@"��@�@��@��@c @�@C@/�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111  ;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oG�O�;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oB
=B
=B
=B
=B
=B
=B	7B
=B	7B
=B
=B
=B
=BDBJBDB
=BJBDBDBJBPBPBVBPBhB�B�B�B!�B$�B5?BA�B>wBT�BdZBq�B�1B�+B�oB��B��B��B�B�RB�^B�wB�jB�qB�^B�LB�9B�3B�B�B��B��B��B��B��B�{B�JB�Bz�Bt�Bq�BjB]/BG�B9XB:^B5?B�B�B+B�B��B�B!�B�BVB��B��B�B�JB|�Bn�BffB]/BM�B;dB,B'�B�B�B{B	7B
��B
��B
�B
�B
�5B
��B
ɺB
�B
��B
�+B
r�B
dZB
ZB
P�B
D�B
2-B
"�B
uB	��B	�#B	ɺB	�-B	��B	�\B	��B	��B	��B	�oB	�hB	��B	��B	��B	��B	�B	�B	�B	��B	��B	��B	��B	��B	�JB	�7B	�%B	{�B	n�B	q�B	t�B	gmB	I�B	@�B	=qB	>wB	E�B	;dB	,B	�B	�B	bB	PB	JB	1B	B��B�B�B�yB�ZB�;B�B��B��B��B��BƨB��B�XB�?B�B��B��B��B��B��B��B�oB�+B�%B�B�B�B� Bz�Bv�Bq�Bq�B�B�PB�B�DB�uB��B��B��B��B�{B�oB�JB�=B�7B�B� Bz�Bv�Bk�BhsBdZB_;B[#BS�BM�BN�BK�BE�BB�B@�B>wB=qB@�B@�B=qB;dB5?B2-B-B+B+B,B,B+B)�B'�B#�B"�B"�B"�B#�B"�B �B�B�B�B�B�B�B�B�B�B�B{B{B{B{BuB�B�B�B�B�B�B�B�B�BhBVBPBJB
=B	7B
=BVBhBuB{BuB{B{B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B!�B"�B#�B$�B#�B#�B$�B$�B%�B(�B+B/B1'B49B6FB6FB5?B7LB=qB?}B?}B?}BA�BC�BG�BI�BL�BL�BK�BR�BXBYBZB[#B]/B`BBhsBiyBk�Bl�Bl�Bm�Bn�Bo�Br�Bv�Bx�B|�B�B�%B�=B�PB�bB�{B��B��B��B��B��B��B��B��B�B�B�!B�'B�?B�RB�^B�jB�qB��B��BÖBÖBĜBƨBȴB��B��B��B��B��B��B��B�
B�/B�BB�NB�fB�sB�B�B�B�B�B�B��B��B��B��B��B��B��B	B	B	B	B	%B	
=B	PB	VB	VB	bB	hB	oB	�B	�B	�B	 �B	#�B	%�B	'�B	+B	.B	/B	2-B	33B	33B	6FB	:^B	>wB	@�B	A�B	B�B	C�B	D�B	E�B	E�B	E�B	G�B	L�B	M�B	O�B	Q�B	R�B	XB	_;B	bNB	cTB	dZB	e`B	e`B	gmB	hsB	jB	k�B	m�B	q�B	r�B	u�B	y�B	{�B	|�B	}�B	~�B	�B	�B	�B	�B	�B	�%B	�%B	�+B	�1B	�=B	�PB	�PB	�PB	�\B	�hB	�{B	�{B	�{B	��B	��B	��B	�B	خB	�CB
�B
]B
'RB
0�B
4�B
;dB
@OB
EmB
J�B
O�B
W�B
^�B
e�B
k�B
qB
t�B
yX111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111  B
B
B
B
B
B
B	B
B	B
B
B
B
B B'B"B
B+B"B B+B.B,B4B,BIBbB�B�B!�B$�B5BAgB>TBT�Bd7Bq�B�B�B�LB��B��B��B��B�/B�>B�VB�HB�LB�>B�%B�B�B��B��B��B��B��B�dB�oB�YB�$B��Bz�Bt�Bq�Bj\B]	BG�B90B:9B5B�BpBB�B��BoB!�BnB.B��BоB��B�'B|�BnqBfBB]	BM�B;=B+�B'�B�B�BSB	B
��B
��B
�|B
�cB
�B
��B
ɒB
��B
�xB
�B
r�B
d/B
Y�B
P�B
DsB
2B
"�B
LB	��B	��B	ɒB	�B	��B	�2B	�pB	�oB	�\B	�EB	�<B	�nB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�VB	�B	�
B	��B	{�B	nmB	qB	t�B	gBB	I�B	@XB	=EB	>LB	EyB	;8B	+�B	xB	WB	5B	%B	B	B	�B��B�B�fB�LB�.B�B��BеB̠BʓBʕB�}B�UB�.B�B��B��B��B��B��B��B�wB�AB��B��B��B��B��B�Bz�Bv�Bq}Bq{B��B�!B��B�B�GB��B�}B�kB�QB�MB�AB�B�B�B��B�Bz�Bv�BkWBhCBd*B_BZ�BS�BM�BN�BK�BEsBB_B@UB>GB=AB@TB@SB=?B;5B5B1�B,�B*�B*�B+�B+�B*�B)�B'�B#�B"�B"�B"�B#�B"�B �B�B{BuB}B|BtBcB\BPBRBIBIBJBJBCBiB[BXBfBpBtBgB\BPB5B&BBB
	B	B
B$B6BBBIB@BFBHBYBfB_B[BSBTBfBjBlBtBtBlBzBBxB�B�B�B�B!�B"�B#�B$�B#�B#�B$�B$�B%�B(�B*�B.�B0�B4B6B6B5B7B=>B?MB?HB?IBAVBCcBG{BI�BL�BL�BK�BR�BW�BX�BY�BZ�B\�B`BhABiFBkRBlVBlWBm]BneBohBr~Bv�Bx�B|�B��B��B�
B�B�/B�HB�TB�RB�`B�rB��B��B��B��B��B��B��B��B�B�B�+B�5B�=B�MB�UB�`B�aB�hB�qB�BˏB͟B͛BΣBЯB��B��B��B��B�B�B�/B�<B�JB�PB�WB�aB�dB�sB��B��B��B��B��B��B��B	�B	�B	�B	�B	�B	
B	B	"B	!B	,B	2B	:B	WB	eB	|B	 �B	#�B	%�B	'�B	*�B	-�B	.�B	1�B	2�B	2�B	6B	:)B	>CB	@PB	ARB	BZB	CbB	DhB	EmB	EmB	EmB	GuB	L�B	M�B	O�B	Q�B	R�B	W�B	_B	bB	cB	d$B	e*B	e*B	g6B	h@B	jIB	kPB	mZB	qvB	r}B	u�B	y�B	{�B	|�B	}�B	~�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�&B	�4B	�FB	�GB	�FB	�IG�O�B	��B	��B	�{B	�B
�B
(B
'B
0mB
4�B
;1B
@B
E8B
JqB
O�B
W�B
^�B
e{B
k�B
p�B
t�B
y&111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111  <#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
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
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
PRES            TEMP            PSAL            PRES_ADJUSTED = PRES - dP                                                                                                                                                                                                                                       none                                                                                                                                                                                                                                                            PSAL_ADJUSTED = sw_salt( sw_cndr(PSAL,TEMP,PRES), TEMP, PRES_ADJUSTED )                                                                                                                                                                                         dP =-0.4 dbar.                                                                                                                                                                                                                                                  none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            Pressures adjusted by using pressure offset at the sea surface. The quoted error is manufacturer specified accuracy in dbar.                                                                                                                                    The quoted error is manufacturer specified accuracy with respect to ITS-90 at time of laboratory calibration.                                                                                                                                                   No significant salinity drift detected. The quoted error is max[0.01, 1xOWC uncertainty] in PSS-78.                                                                                                                                                             201906040953092019060409530920190604095309  AO  ARCAADJP                                                                    20171115180137    IP                G�O�G�O�G�O�                AO  ARGQQCPL                                                                    20171115180137  QCP$                G�O�G�O�G�O�5F03E           AO  ARGQQCPL                                                                    20171115180137  QCF$                G�O�G�O�G�O�0               UW  ARSQUWQC    WOD & nearby Argo as visual check                               20190604095309  IP                  G�O�G�O�G�O�                