CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       N2014-07-21T23:05:42Z AOML 3.0 creation; 2016-05-31T19:14:36Z UW 3.1 conversion     
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
_FillValue                    �4Argo profile    3.1 1.2 19500101000000  20140721230542  20160531121436  5903736 US ARGO PROJECT                                                 STEPHEN RISER                                                   PRES            TEMP            PSAL               EA   AO  4051_7090_069                   2C  D   APEX                            5368                            041511                          846 @��4��1   @��5π @4�hr� ��e%\(�1   GPS     Primary sampling: mixed [deeper than nominal 985dbar: discrete; nominal 985dbar to surface: 2dbar-bin averaged]                                                                                                                                                    EA   A   A   @�ff@�  A   A   A@  A`  A~ffA�  A�  A�  A�  A�  A�  A�  B   B  B  B  B   B(  B0  B8  B@  BH  BP  BX  B`  Bh  Bp  Bx  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  C   C  C  C  C  C
  C  C  C  C  C  C  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D;��D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DJ��DKy�DL  DL�fDM  DMy�DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dt� Dt� Dy�3D�fD�S3D�� D��fD��D�@ D�|�D���D���D�33D�� D��3D�	�D�P Dڌ�D���D�  D�<�D�D��f111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  @��@��R@��RA\)A?\)A_\)A}A��A��A��A��AϮA߮A�A��B�
B�
B�
B�
B'�
B/�
B7�
B?�
BG�
BO�
BW�
B_�
Bg�
Bo�
Bw�
B�
B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��C��C��C��C��C	��C��C��C��C��C��C��C��C��C��C��C��C!��C#��C%��C'��C)��C+��C-��C/��C1��C3��C5��C7��C9��C;��C=��C?��CA��CC��CE��CG��CI��CK��CM��CO��CQ��CS��CU��CW��CY��C[��C]��C_��Ca��Cc��Ce��Cg��Ci��Ck��Cm��Co��Cq��Cs��Cu��Cw��Cy��C{��C}��C��C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C��C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C��C���C���C���D }qD �qD}qD�qD}qD�qD}qD�qD}qD�qD}qD�qD}qD�qD}qD�qD}qD�qD	}qD	�qD
}qD
�qD}qD�qD}qD�qD}qD�qD}qD�qD}qD�qD}qD�qD}qD�qD}qD�qD}qD�qD}qD�qD}qD�qD}qD�qD}qD�qD}qD�qD}qD�qD}qD�qD}qD�qD}qD�qD}qD�qD}qD�qD}qD�qD }qD �qD!}qD!�qD"}qD"�qD#}qD#�qD$}qD$�qD%}qD%�qD&}qD&�qD'}qD'�qD(}qD(�qD)}qD)�qD*}qD*�qD+}qD+�qD,}qD,�qD-}qD-�qD.}qD.�qD/}qD/�qD0}qD0�qD1}qD1�qD2}qD2�qD3}qD3�qD4}qD4�qD5}qD5�qD6}qD6�qD7}qD7�qD8}qD8�qD9}qD9�qD:}qD:�qD;}qD;�D<}qD<�qD=}qD=�qD>}qD>�qD?}qD?�qD@}qD@�qDA}qDA�qDB}qDB�qDC}qDC�qDD}qDD�qDE}qDE�qDF}qDF�qDG}qDG�qDH}qDH�qDI}qDI�qDJ}qDJ�DKwDK�qDL��DL�qDMwDM�qDN}qDN�qDO}qDO�qDP}qDP�qDQ}qDQ�qDR}qDR�qDS}qDS�qDT}qDT�qDU}qDU�qDV}qDV�qDW}qDW�qDX}qDX�qDY}qDY�qDZ}qDZ�qD[}qD[�qD\}qD\�qD]}qD]�qD^}qD^�qD_}qD_�qD`}qD`�qDa}qDa�qDb}qDb�qDc}qDc�qDd}qDd�qDe}qDe�qDf}qDf�qDg}qDg�qDh}qDh�qDi}qDi�qDj}qDj�qDk}qDk�qDl}qDl�qDm}qDm�qDn}qDn�qDo}qDo�qDp}qDp�qDq}qDq�qDr}qDr�qDs}qDs�qDt}qDt�qDy��D�D�Q�D���D��D��D�>�D�{�D�˅D���D�1�D���D���D�RD�N�Dڋ�D�˅D���D�;�D�RD��111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  @��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��A��mA��yA��yA��A��A��A��A��A��A��A��A��A��yA��yA��A��A��yA��A���Aė�A�%A��AÛ�A�G�A�A���A¥�A�A\A�x�A���A�ffA�VA�5?A���A�Q�A���A�p�A�9XA��A���A��RA���A�r�A�`BA�ZA�K�A�K�A�K�A�G�A�C�A�A�A�=qA�+A���A��#A��A��;A��/A��
A���A���A�A��^A��^A��^A��^A��RA��!A���A��DA�M�A��yA��-A��A���A���A���A��mA�ȴA�r�A��`A�1'A�ƨA�XA��/A�K�A��A�G�A�%A��yA���A��FA��A��A��FA���A��A���A�ffA�+A���A���A�n�A�I�A��+A�Q�A�v�A�O�A��A�r�A�oA���A���A���A�1'A��A�|�A�A��A�1'A��PA�;dA��HA���A��jA��;A���A��wA��!A�K�AA~�Az�!AtQ�Aq\)Am?}AlM�Aj�AhĜAg�Ae�7AcAct�Ab��A`�AZ�AWK�AT�`AT(�AR��AQ��API�AN��AL��AI��AH�\AG�AE�ADA�AC��ACS�ABĜAA��A>�RA=�A<A9�mA8�A8M�A7�A7dZA7C�A6�9A6bA5�PA5?}A4��A4�uA4�A3hsA3VA29XA0��A-33A*�A*$�A)�FA)x�A(�A(�\A(A�A'�A'�wA'�A'��A&I�A#%A!
=A�A5?A�-A�`A�/AJA�jA��A�A�-A�yA~�AJA^5A��A��AO�Az�A��A	��A�9A��A�A��A��A&�AAXA�A��A�Av�An�AffAZAE�A$�Aƨ@�dZ@��9@�I�@�v�@�7L@�  @��#@�I�@�S�@���@�p�@�r�@�v�@�9@�R@�p�@�I�@��
@�~�@�/@�dZ@�+@�-@��@�G�@�"�@��@�Q�@�"�@��@؋D@Դ9@���@�V@���@с@�hs@Л�@ϥ�@θR@�?}@�1'@�b@���@��@ʇ+@�5?@�-@Ɂ@ǍP@��H@Ƨ�@�~�@�$�@ũ�@�&�@�Ĝ@ă@�Z@��@�+@\@���@���@�1@��\@�E�@�5?@��@��h@��/@�1'@��P@�o@�V@��@�x�@�x�@�O�@��9@��@�Q�@�t�@�n�@�E�@�$�@��#@�%@��D@�I�@���@�K�@��@�ȴ@��@���@�V@�r�@���@���@��@��@��@��@�$�@��T@�I�@�ƨ@�dZ@��@���@���@���@���@�-@�@��@�`B@�/@�%@��/@��j@�Q�@��@���@�K�@�+@��\@��#@��7@�`B@�/@��@�9X@��@���@��@��\@�~�@�ff@�M�@�-@�J@�@�@�@�J@���@��-@�p�@�7L@�Ĝ@�1'@�(�@��m@���@��
@��
@��F@�dZ@���@�~�@�^5@�{@��^@��^@��^@��-@���@���@���@��7@�hs@�?}@�/@��`@�z�@�I�@�dZ@���@�M�@��@���@���@��@�@�x�@�`B@�7L@���@�9X@�1@�ƨ@���@�|�@�K�@�33@��@���@��T@��@�p�@�p�@�`B@�X@�V@��9@�z�@�Q�@��@���@�+@��H@�ȴ@�ȴ@��!@�v�@�V@�=q@�J@��T@���@��h@�V@��@�Ĝ@���@�Z@�  @��@�t�@�\)@�K�@�"�@��y@��!@�~�@�^5@�-@��7@��@���@��@��@�(�@��@��m@��m@��;@���@��w@��F@��@�X@�@u?}@j^5@b-@[t�@SdZ@I�#@Co@<j@6V@1��@+��@%�-@  �@��@�T@��@��@��111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  A��mA��yA��yA��A��A��A��A��A��A��A��A��A��yA��yA��A��A��yA��A���Aė�A�%A��AÛ�A�G�A�A���A¥�A�A\A�x�A���A�ffA�VA�5?A���A�Q�A���A�p�A�9XA��A���A��RA���A�r�A�`BA�ZA�K�A�K�A�K�A�G�A�C�A�A�A�=qA�+A���A��#A��A��;A��/A��
A���A���A�A��^A��^A��^A��^A��RA��!A���A��DA�M�A��yA��-A��A���A���A���A��mA�ȴA�r�A��`A�1'A�ƨA�XA��/A�K�A��A�G�A�%A��yA���A��FA��A��A��FA���A��A���A�ffA�+A���A���A�n�A�I�A��+A�Q�A�v�A�O�A��A�r�A�oA���A���A���A�1'A��A�|�A�A��A�1'A��PA�;dA��HA���A��jA��;A���A��wA��!A�K�AA~�Az�!AtQ�Aq\)Am?}AlM�Aj�AhĜAg�Ae�7AcAct�Ab��A`�AZ�AWK�AT�`AT(�AR��AQ��API�AN��AL��AI��AH�\AG�AE�ADA�AC��ACS�ABĜAA��A>�RA=�A<A9�mA8�A8M�A7�A7dZA7C�A6�9A6bA5�PA5?}A4��A4�uA4�A3hsA3VA29XA0��A-33A*�A*$�A)�FA)x�A(�A(�\A(A�A'�A'�wA'�A'��A&I�A#%A!
=A�A5?A�-A�`A�/AJA�jA��A�A�-A�yA~�AJA^5A��A��AO�Az�A��A	��A�9A��A�A��A��A&�AAXA�A��A�Av�An�AffAZAE�A$�Aƨ@�dZ@��9@�I�@�v�@�7L@�  @��#@�I�@�S�@���@�p�@�r�@�v�@�9@�R@�p�@�I�@��
@�~�@�/@�dZ@�+@�-@��@�G�@�"�@��@�Q�@�"�@��@؋D@Դ9@���@�V@���@с@�hs@Л�@ϥ�@θR@�?}@�1'@�b@���@��@ʇ+@�5?@�-@Ɂ@ǍP@��H@Ƨ�@�~�@�$�@ũ�@�&�@�Ĝ@ă@�Z@��@�+@\@���@���@�1@��\@�E�@�5?@��@��h@��/@�1'@��P@�o@�V@��@�x�@�x�@�O�@��9@��@�Q�@�t�@�n�@�E�@�$�@��#@�%@��D@�I�@���@�K�@��@�ȴ@��@���@�V@�r�@���@���@��@��@��@��@�$�@��T@�I�@�ƨ@�dZ@��@���@���@���@���@�-@�@��@�`B@�/@�%@��/@��j@�Q�@��@���@�K�@�+@��\@��#@��7@�`B@�/@��@�9X@��@���@��@��\@�~�@�ff@�M�@�-@�J@�@�@�@�J@���@��-@�p�@�7L@�Ĝ@�1'@�(�@��m@���@��
@��
@��F@�dZ@���@�~�@�^5@�{@��^@��^@��^@��-@���@���@���@��7@�hs@�?}@�/@��`@�z�@�I�@�dZ@���@�M�@��@���@���@��@�@�x�@�`B@�7L@���@�9X@�1@�ƨ@���@�|�@�K�@�33@��@���@��T@��@�p�@�p�@�`B@�X@�V@��9@�z�@�Q�@��@���@�+@��H@�ȴ@�ȴ@��!@�v�@�V@�=q@�J@��T@���@��h@�V@��@�Ĝ@���@�Z@�  @��@�t�@�\)@�K�@�"�@��y@��!@�~�@�^5@�-@��7@��@���@��@��@�(�@��@��m@��m@��;@���@��w@��F@��@�X@�@u?}@j^5@b-@[t�@SdZ@I�#@Co@<j@6V@1��@+��@%�-@  �@��@�T@��@��@��111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  ;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oB�}B�}B�}B�}B�}B�}B�}B�}B��B�}B�}B��B��B��B��B��B��BŢB��B��B7LBF�BS�B\)B\)BXBR�BR�BW
BcTB�%B��B��B��B��B�3B��B��B�B�5B�5B�BB�NB�ZB�ZB�`B�mB�B�B�B�B�B�B�B�`B�yB�B�B��B��B��B��B��B��B��B��B��B��B��B��B��B�B�B�B�ZB��B��B� Bl�BT�BM�BH�BA�B2-B-B\B%B�B��B��B�9B��B��B�{B|�BW
B9XB)�B%�B �B�B�B�BVB��B�mB�#B��B��BƨB�wB�LB�'B��B�1B\)BB�B49B,B&�B�BB
�TB
��B
�XB
�7B
o�B
n�B
m�B
k�B
ffB
_;B
W
B
8RB
oB	��B	�fB	�;B	��B	��B	B	�RB	�B	�B	��B	�uB	w�B	ffB	ZB	S�B	K�B	E�B	<jB	33B	'�B	�B	{B	\B		7B	B	  B��B��B��B�B�fB�HB�)B�B�B�B�B��B��B��B��B��B��B��BɺBǮBƨBB�dB�3B�B�B�B�B��B��B��B��B��B��B��B��B��B��B�uB�bB�VB�DB�%B� Bz�By�Bw�Bv�Bu�Bt�Br�Br�Bq�Bq�Bo�Bm�BjBhsBffBiyBgmBgmBgmBffBffBe`Be`Be`BdZBdZBdZBdZBcTBcTBbNB`BBdZBe`Be`BffBgmBhsBjBn�Bp�Bp�Bp�Bp�Bu�Bx�B|�B~�B�B�B�B�B�=B�PB�VB�VB�\B�uB��B��B��B��B��B�B�FB�RB�XB�dB�dB�qB�}B�}BÖBɺB��B��B��B��B��B��B��B�B�B�B�B�B�#B�)B�/B�5B�5B�5B�BB�HB�NB�TB�ZB�B�B�B�B�B�B��B��B��B��B��B��B��B	  B	B	B	B	B	%B	%B	+B	1B	DB	JB	JB	VB	hB	uB	{B	�B	�B	�B	!�B	#�B	+B	49B	5?B	6FB	6FB	7LB	;dB	H�B	Q�B	VB	ZB	[#B	[#B	[#B	[#B	^5B	`BB	bNB	bNB	cTB	cTB	dZB	dZB	ffB	hsB	iyB	k�B	l�B	o�B	q�B	r�B	s�B	t�B	v�B	|�B	�B	�B	�%B	�1B	�1B	�1B	�7B	�7B	�=B	�=B	�=B	�=B	�=B	�DB	�PB	�VB	�VB	�\B	�bB	�oB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�!B	�'B	�3B	�3B	�3B	�3B	�9B	�9B	�?B	�LB	�RB	�^B	�dB	�qB	��B	ÖB	ÖB	ÖB	ǮB	��B	��B	��B	��B	��B	��B	��B	��B	�B	�
B	�B	�B	�)B	�/B	�)B	�/B	�5B	�;B	�;B	�BB	�BB	�HB	�NB	�ZB	�ZB	�`B	�fB	�fB	�sB	�yB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B
1B
PB
�B
 �B
(�B
.B
5?B
=qB
B�B
H�B
M�B
R�B
XB
\)B
aHB
ffB
k�B
n�B
s�B
w�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��BŪB��B��B7SBF�BS�B\1B\1BXBR�BR�BWBc]B�,B��B��B��B��B�<B��B� B�+B�AB�@B�KB�ZB�eB�fB�iB�xB�B�B�B�B�B�B�B�mB�B�B�B��B��B��B��B��B��B��B��B��B��B��B��B��B�B�B�B�iB��B��B�Bl�BUBM�BH�BA�B22B-BdB,B�B�B��B�BB��B��B�B|�BWB9]B*B%�B �B�B�B�BZB��B�rB�&B��B��BƪB�zB�QB�+B��B�8B\.BB�B4>B,B&�B�BB
�\B
��B
�^B
�=B
o�B
n�B
m�B
k�B
fmB
_FB
WB
8[B
zB	��B	�rB	�GB	� B	��B	 B	�_B	�"B	�B	��B	��B	w�B	fvB	Z0B	TB	K�B	E�B	<{B	3EB	(B	�B	�B	pB		MB	'B	 B�	B��B��B�B�{B�_B�@B�.B�'B�B�B�B�B��B��B��B��B��B��B��BƿBªB�|B�KB�3B�%B�!B�B�B�B�B�B�B��B��B��B��B��B��B�{B�oB�`B�?B�Bz�By�Bw�Bv�Bu�Bt�Br�Br�Bq�Bq�Bo�Bm�Bj�Bh�Bf�Bi�Bg�Bg�Bg�Bf�Bf�Be{BexBe}BdsBduBdvBdvBcqBcpBbiB`_BdtBe{Be{Bf�Bg�Bh�Bj�Bn�Bp�Bp�Bp�Bp�Bu�Bx�B}BB�"B� B�'B�4B�WB�jB�oB�oB�vB��B��B��B��B��B��B�2B�^B�hB�lB�|B�|B��B��B��BíB��B��B��B��B��B�B�	B�B�/B�/B�,B�,B�-B�8B�?B�DB�MB�LB�KB�WB�]B�fB�iB�oB�B��B��B�B��B��B��B��B��B�B�B�B�B	 B	!B	"B	!B	B	8B	9B	>B	EB	WB	^B	^B	iB	{B	�B	�B	�B	�B	�B	!�B	#�B	+B	4JB	5RB	6WB	6WB	7_B	;uB	H�B	Q�B	VB	Z.B	[2B	[5B	[4B	[6B	^FB	`SB	b_B	b`B	ccB	ceB	diB	diB	fuB	h�B	i�B	k�B	l�B	o�B	q�B	r�B	s�B	t�B	v�B	|�B	�B	�B	�4B	�@B	�AB	�AB	�GB	�DB	�NB	�MB	�MB	�MB	�NB	�RB	�_B	�dB	�gB	�iB	�rB	�~B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�B	�B	�B	�#B	�,B	�(B	�0B	�5B	�AB	�BB	�BB	�@B	�GB	�EB	�KB	�ZB	�`B	�lB	�qB	�B	��B	ãB	ãB	âB	ǼB	��B	��B	��B	��B	��B	��B	�B	�	B	�B	�B	�$B	�*B	�7B	�:B	�7B	�<B	�CB	�GB	�HB	�OB	�OB	�VB	�YB	�fB	�gB	�lB	�rB	�qB	�~B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B
;B
ZB
�B
 �B
) B
. B
5JB
={B
B�B
H�B
M�B
R�B
XB
\2B
aQB
fqB
k�B
n�B
s�B
w�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  <#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
PRES            TEMP            PSAL            PRES_ADJUSTED = PRES - dP                                                                                                                                                                                                                                       none                                                                                                                                                                                                                                                            PSAL_ADJUSTED = sw_salt( sw_cndr(PSAL,TEMP,PRES), TEMP, PRES_ADJUSTED )                                                                                                                                                                                         dP =0.04 dbar.                                                                                                                                                                                                                                                  none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            Pressures adjusted by using pressure offset at the sea surface. The quoted error is manufacturer specified accuracy in dbar.                                                                                                                                    The quoted error is manufacturer specified accuracy with respect to ITS-90 at time of laboratory calibration.                                                                                                                                                   No significant salinity drift detected. Salinity adjusted for effects of pressure adjustment. The quoted error is max[0.01, 1xOW uncertainty] in PSS-78.                                                                                                        201605311214362016053112143620160531121436  AO  ARCAADJP                                                                    20140721230542    IP                G�O�G�O�G�O�                AO  ARGQQCPL                                                                    20140721230542  QCP$                G�O�G�O�G�O�DFB7E           AO  ARGQQCPL                                                                    20140721230542  QCF$                G�O�G�O�G�O�0               UW  ARSQUWQC    WOD & nearby Argo as visual check                               20160531121436  IP                  G�O�G�O�G�O�                