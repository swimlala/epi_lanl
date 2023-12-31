CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       N2015-01-20T20:16:49Z AOML 3.0 creation; 2016-05-31T19:14:41Z UW 3.1 conversion     
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
_FillValue                    �4Argo profile    3.1 1.2 19500101000000  20150120201649  20160531121441  5903736 US ARGO PROJECT                                                 STEPHEN RISER                                                   PRES            TEMP            PSAL               eA   AO  4051_7090_101                   2C  D   APEX                            5368                            041511                          846 @�4#�t�1   @�4$P���@4��-�dc�E���1   GPS     Primary sampling: mixed [deeper than nominal 985dbar: discrete; nominal 985dbar to surface: 2dbar-bin averaged]                                                                                                                                                    eA   A   A   @333@�  @�  A   AffA@  A`  A�  A�  A�  A�  A�  A�  A�  A�  B   B  B  B  B   B(  B0  B8  B@  BH  BP  BX  B`  Bh  Bp  Bx  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�33B�  B�  B�  B�  B�  B�  B���B�  B�  B�  B�  B�  B�  B�  B�  C   C  C  C  C  C
  C  C  C  C  C  C  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D��D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� DqfDq�fDr  Dr� Ds  Ds� Dt  Dt� Dt� Dy�3D� D�<�D���D���D�fD�0 D�� D��3D�� D�L�D��3D�� D��D�6fD�y�D�ɚD��D�I�D�f111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  @0��@}p�@��R@��RAA?\)A_\)A\)A��A��A��A��AϮA߮A�A��B�
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
B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B۸RB��B��B��B��B��B��B��B��B��C��C��C��C��C	��C��C��C��C��C��C��C��C��C��C��C��C!��C#��C%��C'��C)��C+��C-��C/��C1��C3��C5��C7��C9��C;��C=��C?��CA��CC��CE��CG��CI��CK��CM��CO��CQ��CS��CU��CW��CY��C[��C]��C_��Ca��Cc��Ce��Cg��Ci��Ck��Cm��Co��Cq��Cs��Cu��Cw��Cy��C{��C}��C��C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���D }qD �qD}qD�qD}qD�qD}qD�qD}qD�qD}qD�qD}qD�qD}qD�qD}qD�qD	}qD	�qD
}qD
�qD}qD�qD}qD�qD}qD�qD}qD�qD}qD�qD}qD�qD}qD�qD}qD�qD}qD�qD}qD�qD}qD�qD}qD�qD}qD�qD}qD�qD}qD�qD}qD�qD}qD�qD}qD�qD}qD�qD}qD�D}qD�qD }qD �qD!}qD!�qD"}qD"�qD#}qD#�qD$}qD$�qD%}qD%�qD&}qD&�qD'}qD'�qD(}qD(�qD)}qD)�qD*}qD*�qD+}qD+�qD,}qD,�qD-}qD-�qD.}qD.�qD/}qD/�qD0}qD0�qD1}qD1�qD2}qD2�qD3}qD3�qD4}qD4�qD5}qD5�qD6}qD6�qD7}qD7�qD8}qD8�qD9}qD9�qD:}qD:�qD;}qD;�qD<}qD<�qD=}qD=�qD>}qD>�qD?}qD?�qD@}qD@�qDA}qDA�qDB}qDB�qDC}qDC�qDD}qDD�qDE}qDE�qDF}qDF�qDG}qDG�qDH}qDH�qDI}qDI�qDJ}qDJ�qDK}qDK�qDL}qDL�qDM}qDM�qDN}qDN�qDO}qDO�qDP}qDP�qDQ}qDQ�qDR}qDR�qDS}qDS�qDT}qDT�qDU}qDU�qDV}qDV�qDW}qDW�qDX}qDX�qDY}qDY�qDZ}qDZ�qD[}qD[�qD\}qD\�qD]}qD]�qD^}qD^�qD_}qD_�qD`}qD`�qDa}qDa�qDb}qDb�qDc}qDc�qDd}qDd�qDe}qDe�qDf}qDf�qDg}qDg�qDh}qDh�qDi}qDi�qDj}qDj�qDk}qDk�qDl}qDl�qDm}qDm�qDn}qDn�qDo}qDo�qDp}qDq�Dq��Dq�qDr}qDr�qDs}qDs�qDt}qDt�qDy��D��D�;�D���D���D�D�.�D�~�D���D��D�K�D���DǾ�D��D�5D�xRD��RD��D�HRD�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  @��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��AǁAǁAǁAǁAǇ+AǅAǃAǃAǁA�z�A�l�A�jA�n�A�n�A�l�A�dZA�dZA�bNA�bNA�S�A�I�A�(�A���AƗ�A�`BA�?}A�$�A�A���A�bNA�"�A�A�&�A��;Aĝ�Aħ�AľwAļjAąA�^5A�Aô9AÑhA�n�A�"�A��A���A�JA�l�A���A�M�A��FA��
A�1A�
=A��HA�S�A���A�7LA��A��A���A�G�A���A�1'A��PA��jA��A�E�A��jA�dZA�%A���A�z�A��A�~�A��#A�9XA�/A�bA�33A���A���A���A�M�A�ȴA�A�A��A�
=A�+A��A�VA�+A�x�A��FA�XA�A�33A���A�
=A��jA�v�A�=qA��A�5?A���A���A���A��A��#A��A���A�C�A��A�7LA�~�A|M�AzffAy�AxJAu
=AsO�Ar~�An��AmO�Al1Ah�Af^5Ad�jAb��Aa�^A`�HA`I�A]"�A[�hAY��AX-AW
=AT��AP��AMAL�AKhsAKAJjAIhsAHr�AG��AF1'AB��A?�A=�A;;dA:VA:1A9�A733A6E�A4ĜA3x�A2��A2^5A0�yA.��A-�
A,=qA*�A(�RA'�-A't�A&�yA%hsA$A"��A ��A�7AS�A��AZAdZA�+A��AoAQ�AJA��A��AĜAS�A%A  A�A|�A�A�^Al�Az�A
�`A
E�A
bA	�
A	�wA	��A	?}A�+A �AK�A��A~�A5?A��A�FA�HA{A�^A�A�mA ��A @�^5@��T@��@�1'@��@�E�@�p�@��@��@�\)@�J@��7@�%@�(�@�ff@�F@���@@�x�@�(�@�@��@��@�\)@�M�@�7@�@�@�-@�$�@��@�@�hs@���@�dZ@ݩ�@ܓu@�1'@�K�@ڇ+@�^5@�7L@�-@���@��`@�n�@�9X@��H@�$�@�/@�Z@��@ˮ@���@��@�/@ǝ�@�n�@�-@�`B@ă@���@\@���@�x�@���@��@� �@��w@���@���@���@��-@���@�?}@��@�  @�ƨ@��P@�"�@�5?@��h@���@��;@�\)@��!@���@�hs@���@���@���@�j@��@�t�@��y@���@�~�@�5?@�@��@��#@��^@��7@�p�@���@�9X@��F@�33@�ȴ@�ff@�-@��@��@��9@�Z@���@��F@�\)@�
=@��y@��!@���@�v�@�V@��@��#@�x�@�G�@�V@���@��/@�j@��@���@�l�@�+@��+@�5?@�@�V@��@��/@���@�A�@�  @���@��m@���@���@�33@��@���@�~�@�ff@�=q@�@���@��@���@���@���@��@�1'@��@���@�|�@�dZ@�C�@���@�ȴ@���@�=q@�{@���@���@�x�@�X@��@��/@��@��j@���@�1'@��@�l�@�"�@�@��@��H@�ȴ@��R@���@��+@�~�@�v�@�V@���@�x�@�7L@���@���@���@���@�r�@�1'@��@��@���@��@��@�\)@�C�@�;d@��@�ff@���@�@��h@�/@���@��/@�Ĝ@���@��@�I�@� �@��m@��@�\)@�;d@�33@�+@��@��@��!@���@��+@�^5@�J@���@�p�@�7L@��@��@�z�@�Q�@��@��F@�|�@�dZ@�l�@�t�@�l�@�"�@���@��@�^5@��@�@��h@�?}@��/@�Z@��;@���@�33@�
=@��y@��!@�=q@���@�bN@���@�Ĝ@w�@q&�@i��@c��@]�@Xb@PA�@H��@?�P@8��@1�#@,z�@%�@ bN@��@@M�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  AǁAǁAǁAǁAǇ+AǅAǃAǃAǁA�z�A�l�A�jA�n�A�n�A�l�A�dZA�dZA�bNA�bNA�S�A�I�A�(�A���AƗ�A�`BA�?}A�$�A�A���A�bNA�"�A�A�&�A��;Aĝ�Aħ�AľwAļjAąA�^5A�Aô9AÑhA�n�A�"�A��A���A�JA�l�A���A�M�A��FA��
A�1A�
=A��HA�S�A���A�7LA��A��A���A�G�A���A�1'A��PA��jA��A�E�A��jA�dZA�%A���A�z�A��A�~�A��#A�9XA�/A�bA�33A���A���A���A�M�A�ȴA�A�A��A�
=A�+A��A�VA�+A�x�A��FA�XA�A�33A���A�
=A��jA�v�A�=qA��A�5?A���A���A���A��A��#A��A���A�C�A��A�7LA�~�A|M�AzffAy�AxJAu
=AsO�Ar~�An��AmO�Al1Ah�Af^5Ad�jAb��Aa�^A`�HA`I�A]"�A[�hAY��AX-AW
=AT��AP��AMAL�AKhsAKAJjAIhsAHr�AG��AF1'AB��A?�A=�A;;dA:VA:1A9�A733A6E�A4ĜA3x�A2��A2^5A0�yA.��A-�
A,=qA*�A(�RA'�-A't�A&�yA%hsA$A"��A ��A�7AS�A��AZAdZA�+A��AoAQ�AJA��A��AĜAS�A%A  A�A|�A�A�^Al�Az�A
�`A
E�A
bA	�
A	�wA	��A	?}A�+A �AK�A��A~�A5?A��A�FA�HA{A�^A�A�mA ��A @�^5@��T@��@�1'@��@�E�@�p�@��@��@�\)@�J@��7@�%@�(�@�ff@�F@���@@�x�@�(�@�@��@��@�\)@�M�@�7@�@�@�-@�$�@��@�@�hs@���@�dZ@ݩ�@ܓu@�1'@�K�@ڇ+@�^5@�7L@�-@���@��`@�n�@�9X@��H@�$�@�/@�Z@��@ˮ@���@��@�/@ǝ�@�n�@�-@�`B@ă@���@\@���@�x�@���@��@� �@��w@���@���@���@��-@���@�?}@��@�  @�ƨ@��P@�"�@�5?@��h@���@��;@�\)@��!@���@�hs@���@���@���@�j@��@�t�@��y@���@�~�@�5?@�@��@��#@��^@��7@�p�@���@�9X@��F@�33@�ȴ@�ff@�-@��@��@��9@�Z@���@��F@�\)@�
=@��y@��!@���@�v�@�V@��@��#@�x�@�G�@�V@���@��/@�j@��@���@�l�@�+@��+@�5?@�@�V@��@��/@���@�A�@�  @���@��m@���@���@�33@��@���@�~�@�ff@�=q@�@���@��@���@���@���@��@�1'@��@���@�|�@�dZ@�C�@���@�ȴ@���@�=q@�{@���@���@�x�@�X@��@��/@��@��j@���@�1'@��@�l�@�"�@�@��@��H@�ȴ@��R@���@��+@�~�@�v�@�V@���@�x�@�7L@���@���@���@���@�r�@�1'@��@��@���@��@��@�\)@�C�@�;d@��@�ff@���@�@��h@�/@���@��/@�Ĝ@���@��@�I�@� �@��m@��@�\)@�;d@�33@�+@��@��@��!@���@��+@�^5@�J@���@�p�@�7L@��@��@�z�@�Q�@��@��F@�|�@�dZ@�l�@�t�@�l�@�"�@���@��@�^5@��@�@��h@�?}@��/@�Z@��;@���@�33@�
=@��y@��!@�=q@���@�bN@���@�Ĝ@w�@q&�@i��@c��@]�@Xb@PA�@H��@?�P@8��@1�#@,z�@%�@ bN@��@@M�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  ;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oBcTBcTBcTBcTBcTBcTBcTBbNBbNBaHB_;B_;B_;B_;B_;B_;B_;B^5B^5B_;B_;BbNBhsBk�BjBk�Bm�Bt�Bq�Bm�Bm�B}�B�uB�VB�hB��B��B��B��B��B��B�RB�qB�qB�wB�jB�XB��B�B��B�Bw�Br�BhsBZBT�BN�BG�BA�B6FB/B%�B�B�B{B\B1BB��B��B�B�B�;B�B��B��BĜB�qB�'B�B��Br�BT�BN�BG�B?}B6FB$�B�BJB��B��B��B�B�sB�BB��B�}B�B��B��B��B�oB�=Bw�BYBG�B6FB�B
�B
��B
�jB
��B
�7B
z�B
m�B
L�B
>wB
5?B
,B
�B
JB
B	�B	�mB	�)B	ɺB	�RB	�B	��B	�oB	�=B	�B	q�B	gmB	]/B	R�B	L�B	=qB	)�B	�B	�B	�B	�B	�B	�B	uB	\B	%B��B�B�`B�)B��B��BɺBĜB��B�jB�RB�FB�3B�B��B��B��B��B��B��B��B��B�uB�bB�VB�PB�VB�PB�JB�JB�DB�=B�7B�+B�+B�B�B�B� B}�B|�Bz�Bx�Bv�Bt�Bq�Bp�Bn�Bm�Bm�Bl�Bl�Bl�Bl�Bk�BjBjBiyBhsBhsBhsBhsBffBe`BffBgmBgmBiyBhsBgmBffBffBiyBffBdZBffBffBffBhsBp�Bm�Bt�Bx�B}�B�B�B�B�B�+B�DB�PB�VB�VB�hB�uB�uB�{B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B�B�B�B�B�-B�RB�jB��BŢBƨBǮBɺB��B��B��B�B�B�#B�;B�NB�yB�B�B�B�B�B�B��B��B��B��B��B	  B	B	%B	+B	1B		7B	JB	VB	VB	hB	uB	�B	�B	�B	�B	!�B	"�B	#�B	%�B	(�B	,B	-B	/B	1'B	2-B	2-B	33B	33B	5?B	5?B	7LB	;dB	=qB	@�B	B�B	E�B	F�B	J�B	L�B	P�B	T�B	XB	ZB	[#B	]/B	^5B	`BB	`BB	aHB	aHB	bNB	dZB	ffB	gmB	hsB	iyB	iyB	l�B	o�B	r�B	s�B	t�B	y�B	z�B	}�B	�B	�B	�%B	�%B	�DB	�VB	�VB	�\B	�\B	�hB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�!B	�'B	�'B	�3B	�9B	�FB	�RB	�RB	�RB	�^B	�jB	�qB	�qB	�wB	��B	B	B	ĜB	ĜB	ŢB	ǮB	ȴB	ɺB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�
B	�
B	�B	�B	�B	�B	�B	�B	�#B	�#B	�#B	�#B	�)B	�5B	�HB	�HB	�HB	�TB	�TB	�ZB	�ZB	�`B	�`B	�fB	�mB	�sB	�yB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
B
B
B
B
B
B
B
B
B
B
B
+B

=B
VB
�B
�B
"�B
&�B
+B
1'B
6FB
;dB
@�B
F�B
M�B
S�B
ZB
^5B
cTB
gmB
l�B
p�B
s�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  Bc^Bc^Bc^Bc\Bc^Bc\Bc^BbVBbXBaRB_CB_CB_CB_CB_CB_?B_EB^;B^;B_BB_BBbTBh|Bk�Bj�Bk�Bm�Bt�Bq�Bm�Bm�B}�B�}B�_B�nB��B��B��B��B�B��B�YB�zB�yB��B�wB�dB��B�B��B�Bw�Br�Bh|BZ#BUBN�BG�BA�B6MB/!B%�B�B�B�BbB6BB��B��B�B�B�@B�!B��B��BĢB�tB�*B�B��Br�BUBN�BG�B?�B6IB$�B�BLB��B��B��B�B�xB�EB��B��B�B��B��B��B�uB�DBw�BYBG�B6JB�B
�B
��B
�rB
��B
�@B
z�B
m�B
L�B
>�B
5KB
,B
�B
VB
%B	�B	�zB	�5B	��B	�aB	�$B	��B	�~B	�NB	�B	q�B	g}B	]@B	SB	L�B	=�B	*B	�B	�B	�B	�B	�B	�B	�B	qB	;B��B�B�tB�?B�B��B��BĲB��B��B�jB�_B�KB�0B�B��B��B��B��B��B��B��B��B��B�pB�jB�oB�iB�eB�cB�`B�WB�QB�EB�DB�9B�/B�&B�B~B}	Bz�Bx�Bv�Bt�Bq�Bp�Bn�Bm�Bm�Bl�Bl�Bl�Bl�Bk�Bj�Bj�Bi�Bh�Bh�Bh�Bh�Bf�Be|Bf�Bg�Bg�Bi�Bh�Bg�Bf�Bf�Bi�Bf�BdtBf�Bf�Bf�Bh�Bp�Bm�Bt�Bx�B~B� B�2B�4B�8B�FB�^B�jB�nB�pB��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B�B��B�B�$B�$B�#B�5B�DB�iB��B��BŸBƾB��B��B��B��B�B�'B�(B�9B�QB�eB�B�B�B�B�B��B��B��B��B�B�B�B	 B	%B	9B	?B	FB		KB	]B	jB	jB	zB	�B	�B	�B	�B	�B	!�B	"�B	#�B	%�B	)B	,B	-!B	/.B	1:B	2>B	2>B	3AB	3HB	5SB	5RB	7_B	;uB	=�B	@�B	B�B	E�B	F�B	J�B	L�B	P�B	UB	X B	Z,B	[5B	]@B	^GB	`RB	`TB	aVB	aWB	b_B	djB	fwB	g{B	h�B	i�B	i�B	l�B	o�B	r�B	s�B	t�B	y�B	z�B	~B	�'B	�-B	�6B	�4B	�RB	�hB	�fB	�lB	�lB	�uB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�%B	�(B	�/B	�5B	�5B	�BB	�EB	�SB	�`B	�^B	�aB	�lB	�xB	��B	�~B	��B	��B	B	B	ĨB	ħB	ůB	ǾB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�B	�$B	�#B	�$B	�*B	�,B	�2B	�1B	�1B	�1B	�7B	�EB	�UB	�UB	�VB	�aB	�aB	�hB	�fB	�mB	�mB	�rB	�zB	�B	�B	�B	�B	�B	��B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B
B
B
B
 B
'B
'B
 B
%B
+B
)B
*B
7B

GB
aB
�B
�B
"�B
&�B
+B
13B
6OB
;oB
@�B
F�B
M�B
TB
Z&B
^>B
c]B
gvB
l�B
p�B
s�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  <#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
PRES            TEMP            PSAL            PRES_ADJUSTED = PRES - dP                                                                                                                                                                                                                                       none                                                                                                                                                                                                                                                            PSAL_ADJUSTED = sw_salt( sw_cndr(PSAL,TEMP,PRES), TEMP, PRES_ADJUSTED )                                                                                                                                                                                         dP =0.04 dbar.                                                                                                                                                                                                                                                  none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            Pressures adjusted by using pressure offset at the sea surface. The quoted error is manufacturer specified accuracy in dbar.                                                                                                                                    The quoted error is manufacturer specified accuracy with respect to ITS-90 at time of laboratory calibration.                                                                                                                                                   No significant salinity drift detected. Salinity adjusted for effects of pressure adjustment. The quoted error is max[0.01, 1xOW uncertainty] in PSS-78.                                                                                                        201605311214412016053112144120160531121441  AO  ARCAADJP                                                                    20150120201649    IP                G�O�G�O�G�O�                AO  ARGQQCPL                                                                    20150120201649  QCP$                G�O�G�O�G�O�DFB7E           AO  ARGQQCPL                                                                    20150120201649  QCF$                G�O�G�O�G�O�0               UW  ARSQUWQC    WOD & nearby Argo as visual check                               20160531121441  IP                  G�O�G�O�G�O�                