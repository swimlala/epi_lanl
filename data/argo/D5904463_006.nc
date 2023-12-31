CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       N2015-02-26T22:14:50Z AOML 3.0 creation; 2016-08-07T22:44:58Z UW 3.1 conversion     
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
_FillValue                    gx   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         degree_Celsius     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  ix   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  qp   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                    yh   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  {h   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                    �`   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  �`   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  0  �X   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                    ��   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                    ��   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                    ��   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  ,  ��   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                    ��   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                    ��   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                    ��   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                    ��   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  @  ��   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                    �   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                    �   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    �   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar         �(   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar         �,   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�        �0   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    �4Argo profile    3.1 1.2 19500101000000  20150226221450  20160807154458  5904463 US ARGO PROJECT                                                 STEPHEN RISER                                                   PRES            TEMP            PSAL               A   AO  5288_9005_006                   2C  D   APEX                            6530                            072314                          846 @���@1   @�����@)��n���cp1&�x�1   GPS     Primary sampling: mixed [deeper than nominal 985dbar: discrete; nominal 985dbar to surface: 2dbar-bin averaged]                                                                                                                                                    A   B   B   @�ff@�  A   A   A@  A`  A�  A�  A�  A�  A�  A�  A�  A�  B   BffB��B��B   B(  B0  B8��B?��BH  BP  BX  B`  Bh  Bp  BxffB�33B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  C   C  C  C  C  C
  C  C  C  C  C  C  C  C�C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D�fD  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dt� DtٚDy��D��D�S3D��3D���D��D�@ D�l�D��fD�3D�C3D�� Dǰ D�	�D�@ DڦfD���D�3D�,�D�l�D��3111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  @�33@���AffA&ffAFffAfffA�33A�33A�33A�33A�33A�33A�33A�33B��B
  B34BfgB!��B)��B1��B:fgBA34BI��BQ��BY��Ba��Bi��Bq��Bz  B�  B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���C ffCffCffCffCffC
ffCffCffCffCffCffCffCffC� CffCffC ffC"ffC$ffC&ffC(ffC*ffC,ffC.ffC0ffC2ffC4ffC6ffC8ffC:ffC<ffC>ffC@ffCBffCDffCFffCHffCJffCLffCNffCPffCRffCTffCVffCXffCZffC\ffC^ffC`ffCbffCdffCfffChffCjffClffCnffCpffCrffCtffCvffCxffCzffC|ffC~ffC�33C�33C�33C�33C�33C�33C�33C�33C�33C�33C�33C�33C�33C�33C�33C�33C�33C�33C�33C�33C�33C�33C�33C�33C�33C�33C�33C�33C�33C�33C�33C�33C�33C�33C�33C�33C�33C�33C�33C�33C�33C�33C�33C�33C�33C�33C�33C�33C�33C�33C�33C�33C�33C�33C�33C�33C�33C�33C�33C�33C�33C�33C�33C�33C�33C�33C�33C�33C�33C�33C�33C�33C�33C�33C�33C�33C�33C�33C�33C�33C�33C�33C�33C�33C�33C�33C�33C�33C�33C�33C�33C�33C�33C�33C�33C�33C�33C�33C�33C�33C�33C�33C�33C�33C�33C�33C�33C�33C�33C�33C�33C�33C�33C�33C�33C�33C�33C�33C�33C�33C�33C�33C�33C�33C�33C�33C�33C�33D �D ��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D	�D	��D
�D
��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D� D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D �D ��D!�D!��D"�D"��D#�D#��D$�D$��D%�D%��D&�D&��D'�D'��D(�D(��D)�D)��D*�D*��D+�D+��D,�D,��D-�D-��D.�D.��D/�D/��D0�D0��D1�D1��D2�D2��D3�D3��D4�D4��D5�D5��D6�D6��D7�D7��D8�D8��D9�D9��D:�D:��D;�D;��D<�D<��D=�D=��D>�D>��D?�D?��D@�D@��DA�DA��DB�DB��DC�DC��DD�DD��DE�DE��DF�DF��DG�DG��DH�DH��DI�DI��DJ�DJ��DK�DK��DL�DL��DM�DM��DN�DN��DO�DO��DP�DP��DQ�DQ��DR�DR��DS�DS��DT�DT��DU�DU��DV�DV��DW�DW��DX�DX��DY�DY��DZ�DZ��D[�D[��D\�D\��D]�D]��D^�D^��D_�D_��D`�D`��Da�Da��Db�Db��Dc�Dc��Dd�Dd��De�De��Df�Df��Dg�Dg��Dh�Dh��Di�Di��Dj�Dj��Dk�Dk��Dl�Dl��Dm�Dm��Dn�Dn��Do�Do��Dp�Dp��Dq�Dq��Dr�Dr��Ds�Ds��Dt�Dt��Dt�4Dy�4D��D�` D�� D��gD�&gD�L�D�y�D��3D�  D�P D���DǼ�D�gD�L�Dڳ3D�ٚD� D�9�D�y�D�� 111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  @��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��A�hsA�ffA�hsA�l�A�hsA�l�A�r�A�p�A�r�A�t�A�t�A�t�A�t�A�v�A�v�A�p�A�bNA�VA�p�Aԇ+AҰ!A�VA�oA���A��A͑hA�?}A��A�%A��A�K�Aģ�A�JA���A��A��A���A�n�A�jA��yA�K�A���A�(�A�  A�
=A�ȴA��A���A�7LA�`BA�Q�A�+A��A���A�1'A�"�A�K�A��wA���A�A�
=A�p�A��A�7LA��A��A�C�A�VA�?}A�?}A���A�l�A~~�Az�Ast�Ak7LAe��Ab�\A`  A[%AW�AV1'AT{AM�;AHȴAF1ACC�A=��A9��A8(�A7�FA7+A6�jA6I�A6�A7dZA65?A5�-A5/A5�;A6��A6{A4$�A3`BA3
=A3��A2ĜA0I�A+�hA)t�A(�/A(E�A'��A'l�A'`BA'%A&�!A&M�A%7LA#\)A"�A"�!A!ƨA ��A �DA �A��Ap�A�/A�yA�\AA�A�;A7LAĜA��A�HA�A�\Av�An�A$�A�A1A�FAS�A%A{AS�A�Az�A`BA��AbNAI�A1A�FAS�A"�A�DAZA1A��A��A�hAx�AK�A33AoA�9AjA �Ap�A�A�9A��A~�AQ�A�A��Al�A7LA�A�uAffA�DA�\AffAffA�\A�9A�A�A��A��A�+AbNAA�A1'A�A  A�mA�TA�#A��A�Az�AA�;A��A��AC�A
�A
��A
�!A
�A
E�A	��A	x�A	"�A�A��AVA�A�wA��AoAA�A�FAO�A�A�HA��A��Av�A-A�AdZA
=A�A�+AjA^5A1'AƨAO�A �`A �A �!A r�A @�v�@�-@�$�@�-@���@�G�@���@���@���@�$�@��#@��-@���@� �@��w@��!@��@�@�x�@�G�@��@�  @�@�!@��@�^5@�=q@�{@�x�@�j@�
=@�@��^@�X@�j@��@�
=@�$�@���@���@�J@���@陚@�/@�  @�l�@�C�@�
=@�V@�{@���@�x�@���@�(�@�dZ@�n�@�^@���@ߝ�@��@ޟ�@�=q@ݲ-@�?}@�j@ۍP@�5?@�`B@�O�@�&�@ج@��m@�o@֏\@��@Չ7@�&�@��@ԋD@�Z@�I�@���@ҸR@�-@�/@���@���@Гu@�bN@�b@ϝ�@�S�@��@�"�@�S�@�dZ@�o@�V@���@�J@�`B@��/@��
@�dZ@�
=@��H@�v�@ə�@���@ȃ@�Q�@�9X@��
@�
=@�=q@��@�@�X@���@ēu@���@�\)@�C�@�33@§�@��@�X@�&�@��/@�I�@�  @���@���@�V@��T@���@�r�@��@�K�@�33@��@���@�M�@�J@��@���@�O�@��/@��D@��@�C�@��@��+@��@���@�X@�/@��@���@�Z@�1@���@��@��\@��@�O�@��@�z�@�(�@���@�@�=q@��#@���@��h@�hs@�%@��/@���@�Q�@� �@��m@���@�+@�@���@�5?@�J@��-@�G�@��`@��@�z�@�I�@�b@��@�o@��R@�$�@���@�X@��@��@�A�@�  @��@��P@�K�@���@�$�@��T@�%@���@�Q�@�\)@��@���@�$�@��h@�/@�Ĝ@�b@��@�ȴ@���@�G�@�/@��@��/@��@� �@�1@��m@��P@�+@��@�^5@��T@�=q@�|�@��h@�Ĝ@w�@m�T@d9X@\��@Q�@HbN@C�m@>5?@7;d@1��@)X@"~�@�@�^@�/@l�@�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111  A�hsA�ffA�hsA�l�A�hsA�l�A�r�A�p�A�r�A�t�A�t�A�t�A�t�A�v�A�v�A�p�A�bNA�VA�p�Aԇ+AҰ!A�VA�oA���A��A͑hA�?}A��A�%A��A�K�Aģ�A�JA���A��A��A���A�n�A�jA��yA�K�A���A�(�A�  A�
=A�ȴA��A���A�7LA�`BA�Q�A�+A��A���A�1'A�"�A�K�A��wA���A�A�
=A�p�A��A�7LA��A��A�C�A�VA�?}A�?}A���A�l�A~~�Az�Ast�Ak7LAe��Ab�\A`  A[%AW�AV1'AT{AM�;AHȴAF1ACC�A=��A9��A8(�A7�FA7+A6�jA6I�A6�A7dZA65?A5�-A5/A5�;A6��A6{A4$�A3`BA3
=A3��A2ĜA0I�A+�hA)t�A(�/A(E�A'��A'l�A'`BA'%A&�!A&M�A%7LA#\)A"�A"�!A!ƨA ��A �DA �A��Ap�A�/A�yA�\AA�A�;A7LAĜA��A�HA�A�\Av�An�A$�A�A1A�FAS�A%A{AS�A�Az�A`BA��AbNAI�A1A�FAS�A"�A�DAZA1A��A��A�hAx�AK�A33AoA�9AjA �Ap�A�A�9A��A~�AQ�A�A��Al�A7LA�A�uAffA�DA�\AffAffA�\A�9A�A�A��A��A�+AbNAA�A1'A�A  A�mA�TA�#A��A�Az�AA�;A��A��AC�A
�A
��A
�!A
�A
E�A	��A	x�A	"�A�A��AVA�A�wA��AoAA�A�FAO�A�A�HA��A��Av�A-A�AdZA
=A�A�+AjA^5A1'AƨAO�A �`A �A �!A r�A @�v�@�-@�$�@�-@���@�G�@���@���@���@�$�@��#@��-@���@� �@��w@��!@��@�@�x�@�G�@��@�  @�@�!@��@�^5@�=q@�{@�x�@�j@�
=@�@��^@�X@�j@��@�
=@�$�@���@���@�J@���@陚@�/@�  @�l�@�C�@�
=@�V@�{@���@�x�@���@�(�@�dZ@�n�@�^@���@ߝ�@��@ޟ�@�=q@ݲ-@�?}@�j@ۍP@�5?@�`B@�O�@�&�@ج@��m@�o@֏\@��@Չ7@�&�@��@ԋD@�Z@�I�@���@ҸR@�-@�/@���@���@Гu@�bN@�b@ϝ�@�S�@��@�"�@�S�@�dZ@�o@�V@���@�J@�`B@��/@��
@�dZ@�
=@��H@�v�@ə�@���@ȃ@�Q�@�9X@��
@�
=@�=q@��@�@�X@���@ēu@���@�\)@�C�@�33@§�@��@�X@�&�@��/@�I�@�  @���@���@�V@��T@���@�r�@��@�K�@�33@��@���@�M�@�J@��@���@�O�@��/@��D@��@�C�@��@��+@��@���@�X@�/@��@���@�Z@�1@���@��@��\@��@�O�@��@�z�@�(�@���@�@�=q@��#@���@��h@�hs@�%@��/@���@�Q�@� �@��m@���@�+@�@���@�5?@�J@��-@�G�@��`@��@�z�@�I�@�b@��@�o@��R@�$�@���@�X@��@��@�A�@�  @��@��P@�K�@���@�$�@��T@�%@���@�Q�@�\)@��@���@�$�@��h@�/@�Ĝ@�b@��@�ȴ@���@�G�@�/@��@��/@��@� �@�1@��m@��P@�+@��@�^5G�O�@�=q@�|�@��h@�Ĝ@w�@m�T@d9X@\��@Q�@HbN@C�m@>5?@7;d@1��@)X@"~�@�@�^@�/@l�@�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111  ;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oG�O�;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oB
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
�B
�B�B=qBC�BG�BJ�BI�BS�BZBiyBcTBq�B�B��B�NB�B<jBVBaHB� B�B�\B�uB�%B�B�B�PB{�Bn�B[#BM�BF�B:^B1'B#�BVB��B�NB��B�?B��B�VBp�B`BB@�BhB
�B
�;B
�;B
�?B
}�B
M�B
7LB
#�B	��B	�B	�^B	�=B	[#B	7LB	&�B	!�B	)�B	6FB	K�B	_;B	\)B	H�B	>wB	5?B	,B	1'B	I�B	O�B	W
B	o�B	�{B	��B	��B

=B
bB
�B
C�B
dZB
jB
\)B
VB
W
B
k�B
gmB
YB
49B
$�B
 �B
-B
9XB
:^B
>wB
P�B
P�B
O�B
F�B
9XB
A�B
H�B
F�B
G�B
K�B
K�B
J�B
K�B
L�B
VB
VB
T�B
R�B
Q�B
P�B
Q�B
]/B
bNB
iyB
jB
k�B
jB
n�B
p�B
o�B
l�B
jB
aHB
ZB
XB
S�B
N�B
K�B
J�B
I�B
H�B
H�B
J�B
J�B
L�B
L�B
L�B
L�B
L�B
L�B
L�B
M�B
M�B
N�B
O�B
O�B
P�B
P�B
N�B
O�B
O�B
O�B
O�B
O�B
O�B
O�B
O�B
O�B
P�B
Q�B
XB
YB
ZB
\)B
aHB
ffB
m�B
n�B
o�B
n�B
n�B
m�B
l�B
m�B
m�B
l�B
l�B
l�B
l�B
l�B
l�B
jB
hsB
gmB
ffB
dZB
aHB
_;B
^5B
aHB
bNB
aHB
_;B
\)B
ZB
W
B
T�B
R�B
P�B
P�B
N�B
K�B
I�B
G�B
F�B
E�B
E�B
E�B
D�B
D�B
C�B
C�B
B�B
A�B
@�B
?}B
?}B
?}B
>wB
>wB
<jB
:^B
=qB
>wB
=qB
;dB
7LB
7LB
:^B
;dB
;dB
:^B
;dB
:^B
8RB
8RB
7LB
6FB
5?B
33B
2-B
1'B
1'B
2-B
1'B
1'B
0!B
1'B
/B
.B
/B
/B
/B
/B
/B
.B
-B
,B
+B
)�B
(�B
'�B
&�B
&�B
&�B
&�B
&�B
&�B
&�B
%�B
%�B
%�B
%�B
$�B
%�B
%�B
%�B
%�B
$�B
#�B
"�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
{B
{B
�B
�B
�B
�B
�B
{B
{B
�B
�B
{B
{B
oB
oB
hB
hB
hB
bB
bB
bB
bB
\B
hB
uB
{B
uB
bB
\B
hB
bB
\B
VB
PB
PB
JB
JB
JB
JB
JB
JB
JB
JB
JB
JB
JB
JB
JB
JB
DB
	7B
	7B
1B
1B
+B
1B
1B
1B
	7B
	7B
	7B

=B

=B

=B

=B
DB
DB
JB
PB
PB
PB
PB
PB
PB
PB
PB
PB
PB
PB
VB
VB
VB
\B
\B
\B
\B
\B
\B
\B
\B
\B
\B
\B
\B
VB
\B
hB
hB
hB
bB
bB
hB
hB
oB
hB
hB
hB
hB
hB
hB
oB
oB
oB
uB
uB
uB
uB
{B
uB
{B
{B
{B
{B
{B
{B
{B
{B
{B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
 �B
"�B
%�B
,B
2-B
7LB
>wB
C�B
I�B
M�B
S�B
ZB
]/B
_;B
dZB
gmB
l�B
q�B
t�B
w�B
{�B
�B
�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111  B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
�B�B=RBCuBG�BJ�BI�BS�BZ BiXBc5Bq�B��B��B�-BuB<GBU�Ba#B�B��B�=B�TB�B��B��B�0B{�BntB[BM�BF�B::B1 B#�B0B��B�*BʜB�B��B�/Bp�B`B@_BDB
�~B
�B
�B
�B
}�B
M�B
7+B
#�B	��B	��B	�>B	� B	[B	71B	&�B	!�B	)�B	6)B	K�B	_ B	\B	H�B	>WB	5!B	+�B	1B	I�B	O�B	V�B	o|B	�XB	оB	��B

B
<B
�B
CpB
d2B
jVB
[�B
U�B
V�B
k[B
gCB
X�B
4B
$�B
 �B
,�B
90B
:5B
>OB
P�B
P�B
O�B
F~B
9.B
A_B
H�B
F}B
G�B
K�B
K�B
J�B
K�B
L�B
U�B
U�B
T�B
R�B
Q�B
P�B
Q�B
]B
b"B
iNB
jRB
kZB
jVB
nnB
pzB
orB
l_B
jTB
aB
Y�B
W�B
S�B
N�B
K�B
J�B
I�B
H�B
H�B
J�B
J�B
L�B
L�B
L�B
L�B
L�B
L�B
L�B
M�B
M�B
N�B
O�B
O�B
P�B
P�B
N�B
O�B
O�B
O�B
O�B
O�B
O�B
O�B
O�B
O�B
P�B
Q�B
W�B
X�B
Y�B
[�B
aB
f:B
mbB
nlB
osB
nkB
nkB
meB
l`B
mfB
meB
l^B
l`B
l`B
l`B
l`B
l`B
jUB
hGB
gAB
f:B
d.B
aB
_B
^B
aB
b#B
aB
_B
[�B
Y�B
V�B
T�B
R�B
P�B
P�B
N�B
K�B
I�B
G�B
F}B
EwB
EvB
EwB
DqB
DoB
CkB
CjB
BeB
A_B
@XB
?QB
?SB
?RB
>LB
>JB
<?B
:3B
=FB
>MB
=EB
;7B
7!B
7"B
:1B
;:B
;:B
:3B
;;B
:0B
8*B
8&B
7B
6B
5B
3	B
2B
0�B
0�B
2B
0�B
0�B
/�B
0�B
.�B
-�B
.�B
.�B
.�B
.�B
.�B
-�B
,�B
+�B
*�B
)�B
(�B
'�B
&�B
&�B
&�B
&�B
&�B
&�B
&�B
%�B
%�B
%�B
%�B
$�B
%�B
%�B
%�B
%�B
$�B
#�B
"�B
�B
�B
{B
mB
hB
fB
`B
bB
bB
\B
[B
ZB
]B
]B
UB
NB
OB
SB
UB
UB
WB
UB
PB
MB
VB
TB
QB
QB
BB
@B
<B
9B
?B
7B
4B
5B
7B
.B
=B
IB
MB
JB
8B
1B
=B
8B
1B
*B
"B
%B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
	B
	
B
B
B
B
B
B
B
	B
	
B
	B

B

B

B

B
B
B
B
$B
&B
#B
"B
$B
%B
"B
%B
"B
#B
"B
)B
)B
(B
/B
1B
0B
1B
0B
0B
1B
.B
/B
0B
0B
/B
'B
0B
;B
<B
:B
6B
5B
9B
;B
@B
<B
:B
<B
;B
=B
;B
AB
AB
BB
IB
GB
IB
HB
OB
HB
MB
LB
OB
MB
NB
MB
NB
MB
NB
SB
YB
ZB
XB
_B
fB
gB
mB
mB
mB
gB
fB
fB
qB
kB
kB
kB
rB
rB
tB
yB
xB
yB
zB
xB
|B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
 �G�O�B
%�B
+�B
1�B
7B
>FB
CgB
I�B
M�B
S�B
Y�B
\�B
_B
d*B
g?B
l\B
qzB
t�B
w�B
{�B
��B
��111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111  <#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
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
PRES            TEMP            PSAL            PRES_ADJUSTED = PRES - dP                                                                                                                                                                                                                                       none                                                                                                                                                                                                                                                            PSAL_ADJUSTED = sw_salt( sw_cndr(PSAL,TEMP,PRES), TEMP, PRES_ADJUSTED )                                                                                                                                                                                         dP =-0.4 dbar.                                                                                                                                                                                                                                                  none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            Pressures adjusted by using pressure offset at the sea surface. The quoted error is manufacturer specified accuracy in dbar.                                                                                                                                    The quoted error is manufacturer specified accuracy with respect to ITS-90 at time of laboratory calibration.                                                                                                                                                   No significant salinity drift detected. Salinity adjusted for effects of pressure adjustment. The quoted error is max[0.01, 1xOW uncertainty] in PSS-78.                                                                                                        201608071544582016080715445820160807154458  AO  ARCAADJP                                                                    20150226221450    IP                G�O�G�O�G�O�                AO  ARGQQCPL                                                                    20150226221450  QCP$                G�O�G�O�G�O�DFB5E           AO  ARGQQCPL                                                                    20150226221450  QCF$                G�O�G�O�G�O�0               UW  ARSQUWQC    WOD & nearby Argo as visual check                               20160807154458  IP                  G�O�G�O�G�O�                