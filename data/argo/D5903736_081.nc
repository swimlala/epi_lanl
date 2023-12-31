CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       N2014-07-21T23:05:48Z AOML 3.0 creation; 2016-05-31T19:14:38Z UW 3.1 conversion     
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
resolution        :�o     �  U|   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                    ]p   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  _p   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                    gd   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         degree_Celsius     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  id   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  qX   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                    yL   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  {L   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                    �@   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  �@   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  0  �4   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                    �d   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                    �d   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                    �d   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  ,  �d   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                    ��   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                    ��   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                    ��   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                    ��   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  @  ��   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                    ��   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                    ��   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    ��   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar         �   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar         �   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�        �   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    �Argo profile    3.1 1.2 19500101000000  20140721230548  20160531121438  5903736 US ARGO PROJECT                                                 STEPHEN RISER                                                   PRES            TEMP            PSAL               QA   AO  4051_7090_081                   2C  D   APEX                            5368                            041511                          846 @� �� 1   @� 蔫�@2�������e�t�j1   GPS     Primary sampling: mixed [deeper than nominal 985dbar: discrete; nominal 985dbar to surface: 2dbar-bin averaged]                                                                                                                                                    QA   A   A   @���@�  A   A   A@  A`  A�  A�  A�  A�  A�  A�  A�  A�  B   B  B  B  B   B(ffB/��B8  B@  BH  BP  BX  B`  Bh  Bp  Bx  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  C   C  C  C  C  C
  C  C  C  C  C  C  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ�C\  C^  C`  Cb  Cd�Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dts3Dy� D�3D�P D�C3D��fD�fD�9�D���D��fD�3D�FfD�y�D�� D�  D�9�Dڀ D��3D�fD�FfD� D���11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   @�Q�@��R@��RA\)A?\)A_\)A\)A��A��A��A��AϮA߮A�A��B�
B�
B�
B�
B(=pB/p�B7�
B?�
BG�
BO�
BW�
B_�
Bg�
Bo�
Bw�
B�
B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��C��C��C��C��C	��C��C��C��C��C��C��C��C��C��C��C��C!��C#��C%��C'��C)��C+��C-��C/��C1��C3��C5��C7��C9��C;��C=��C?��CA��CC��CE��CG��CI��CK��CM��CO��CQ��CS��CU��CW��CZ]C[��C]��C_��Ca��Cd]Ce��Cg��Ci��Ck��Cm��Co��Cq��Cs��Cu��Cw��Cy��C{��C}��C��C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���D }qD �qD}qD�qD}qD�qD}qD�qD}qD�qD}qD�qD}qD�qD}qD�qD}qD�qD	}qD	�qD
}qD
�qD}qD�qD}qD�qD}qD�qD}qD�qD}qD�qD}qD�qD}qD�qD}qD�qD}qD�qD}qD�qD}qD�qD}qD�qD}qD�qD}qD�qD}qD�qD}qD�qD}qD�qD}qD�qD}qD�qD}qD�qD}qD�qD }qD �qD!}qD!�qD"}qD"�qD#}qD#�qD$}qD$�qD%}qD%�qD&}qD&�qD'}qD'�qD(}qD(�qD)}qD)�qD*}qD*�qD+}qD+�qD,}qD,�qD-}qD-�qD.}qD.�qD/}qD/�qD0}qD0�qD1}qD1�qD2}qD2�qD3}qD3�qD4}qD4�qD5}qD5�qD6}qD6�qD7}qD7�qD8}qD8�qD9}qD9�qD:}qD:�qD;}qD;�qD<}qD<�qD=}qD=�qD>}qD>�qD?}qD?�qD@}qD@�qDA}qDA�qDB}qDB�qDC}qDC�qDD}qDD�qDE}qDE�qDF}qDF�qDG}qDG�qDH}qDH�qDI}qDI�qDJ}qDJ�qDK}qDK�qDL}qDL�qDM}qDM�qDN}qDN�qDO}qDO�qDP}qDP�qDQ}qDQ�qDR}qDR�qDS}qDS�qDT}qDT�qDU}qDU�qDV}qDV�qDW}qDW�qDX}qDX�qDY}qDY�qDZ}qDZ�qD[}qD[�qD\}qD\�qD]}qD]�qD^}qD^�qD_}qD_�qD`}qD`�qDa}qDa�qDb}qDb�qDc}qDc�qDd}qDd�qDe}qDe�qDf}qDf�qDg}qDg�qDh}qDh�qDi}qDi�qDj}qDj�qDk}qDk�qDl}qDl�qDm}qDm�qDn}qDn�qDo}qDo�qDp}qDp�qDq}qDq�qDr}qDr�qDs}qDs�qDtp�Dy}qD��D�N�D�A�D��D�D�8RD���D��D��D�ED�xRD�θD���D�8RD�~�D���D�D�ED�~�D��R11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   @��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��A�O�A�O�A�O�A�O�A�Q�A�S�A�M�A�M�A�I�A�;dA�33A�bA���A�jA���A��`A�jA׸RA�
=AօA�|�A�  A��TA���Aѣ�A���A�S�A�S�A��;A�7LA��A�~�AŃAĝ�A¬A��A��A�
=A�~�A�t�A�bA�=qA��wA�XA�%A��hA�S�A��A�&�A�r�A�/A��TA��A�;dA�
=A��
A��A�O�A��A���A��uA�ȴA���A��FA���A�VA���A��jA�5?A��A���A���A�r�A��#A�
=A���A��A�&�A��PA�?}A��`A��-A���A�;dA���A��;A���A��7A�33A�bNA�Q�A��A�bNA�A�A�p�A�{A�dZA�^5A��A���A�XA�33A�|�A��yA��mA�?}A� �A�A��yA�9XA��uA���A�bA���A��A�`BA�9XA~�A|�uA{33Az�Ay7LAxQ�Aw/As�wAr~�Aq�Ao�Ao��AoAoO�An{Am��Al��AkhsAjbNAi+Ah��Ag��Ae��Ab  A]"�A\AZ=qAX  AV�+AT�AQ�AQ��AQoAN�AK��AJ��AJ~�AHADjAA�7A>�yA<z�A:VA85?A7&�A5�TA45?A3�A2jA0��A/l�A-��A,�jA,5?A)hsA)"�A(��A&ȴA$�jA"5?A r�A�\A�
A"�A�9A  AĜA�TA|�AA�DAI�A��A��AJA��A�A(�AA�A�AĜAz�A�^A�`A�A�AG�A
��A
��A
I�A
�A	��A��AbA�A�RA-A33A�yA9XA�#A�A ��A b@�S�@���@���@�/@�33@�-@��#@�`B@��u@�A�@��@��@���@��@�J@�/@��@�;d@�\@���@�\@�ƨ@�
=@�E�@��
@�P@�|�@�33@�\@�@��@���@���@���@��@���@���@��@��T@��@�p�@�G�@��@��@�j@���@� �@��@ߥ�@�|�@��@�~�@��#@�/@�Z@�dZ@��y@ڰ!@���@�%@�  @�dZ@�33@�o@�5?@ӶF@�ȴ@�E�@�{@��T@ѡ�@���@� �@υ@���@ͺ^@���@� �@˕�@�ȴ@�x�@��m@�;d@Ə\@���@öF@§�@�{@���@��7@�O�@�7L@�V@�Ĝ@�r�@��;@��@��y@�E�@��@�@��@��@���@��@�I�@�9X@�(�@� �@�b@�1@�ƨ@���@�n�@�&�@�Z@�  @�o@�~�@�5?@�J@��h@��/@���@��@�1'@��;@���@�C�@��@�n�@�$�@���@�@��@�/@��j@�(�@��P@��#@�p�@�X@�G�@�?}@�7L@��@��`@��D@��m@�l�@�
=@�v�@�V@�M�@�5?@�@���@�x�@�X@�?}@���@��@�j@�A�@�1@�t�@�+@�o@���@��@��y@��!@�ff@�$�@��h@���@��9@�j@�1@��F@�S�@��@�~�@�$�@��T@���@���@��`@�1'@�1@��;@��@�|�@�l�@�
=@��R@���@�~�@��@�@��@��@�r�@��;@��@�+@���@�n�@��^@���@� �@�|�@�S�@�K�@�\)@�+@���@�{@�X@�/@��@�%@���@��9@�j@��@�S�@�"�@�o@��@���@���@�ff@�J@��#@���@���@���@���@�%@��`@��u@�A�@��
@��P@�dZ@�l�@�dZ@�"�@��H@���@��R@��!@���@�v�@��#@��7@�7L@��`@���@��@��@�C�@�
=@�@��H@���@�V@{ƨ@tZ@lz�@d��@XA�@O��@G
=@>��@6$�@/�w@(��@"~�@��@�\@ȴ@dZ@  @�D@��11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   A�O�A�O�A�O�A�O�A�Q�A�S�A�M�A�M�A�I�A�;dA�33A�bA���A�jA���A��`A�jA׸RA�
=AօA�|�A�  A��TA���Aѣ�A���A�S�A�S�A��;A�7LA��A�~�AŃAĝ�A¬A��A��A�
=A�~�A�t�A�bA�=qA��wA�XA�%A��hA�S�A��A�&�A�r�A�/A��TA��A�;dA�
=A��
A��A�O�A��A���A��uA�ȴA���A��FA���A�VA���A��jA�5?A��A���A���A�r�A��#A�
=A���A��A�&�A��PA�?}A��`A��-A���A�;dA���A��;A���A��7A�33A�bNA�Q�A��A�bNA�A�A�p�A�{A�dZA�^5A��A���A�XA�33A�|�A��yA��mA�?}A� �A�A��yA�9XA��uA���A�bA���A��A�`BA�9XA~�A|�uA{33Az�Ay7LAxQ�Aw/As�wAr~�Aq�Ao�Ao��AoAoO�An{Am��Al��AkhsAjbNAi+Ah��Ag��Ae��Ab  A]"�A\AZ=qAX  AV�+AT�AQ�AQ��AQoAN�AK��AJ��AJ~�AHADjAA�7A>�yA<z�A:VA85?A7&�A5�TA45?A3�A2jA0��A/l�A-��A,�jA,5?A)hsA)"�A(��A&ȴA$�jA"5?A r�A�\A�
A"�A�9A  AĜA�TA|�AA�DAI�A��A��AJA��A�A(�AA�A�AĜAz�A�^A�`A�A�AG�A
��A
��A
I�A
�A	��A��AbA�A�RA-A33A�yA9XA�#A�A ��A b@�S�@���@���@�/@�33@�-@��#@�`B@��u@�A�@��@��@���@��@�J@�/@��@�;d@�\@���@�\@�ƨ@�
=@�E�@��
@�P@�|�@�33@�\@�@��@���@���@���@��@���@���@��@��T@��@�p�@�G�@��@��@�j@���@� �@��@ߥ�@�|�@��@�~�@��#@�/@�Z@�dZ@��y@ڰ!@���@�%@�  @�dZ@�33@�o@�5?@ӶF@�ȴ@�E�@�{@��T@ѡ�@���@� �@υ@���@ͺ^@���@� �@˕�@�ȴ@�x�@��m@�;d@Ə\@���@öF@§�@�{@���@��7@�O�@�7L@�V@�Ĝ@�r�@��;@��@��y@�E�@��@�@��@��@���@��@�I�@�9X@�(�@� �@�b@�1@�ƨ@���@�n�@�&�@�Z@�  @�o@�~�@�5?@�J@��h@��/@���@��@�1'@��;@���@�C�@��@�n�@�$�@���@�@��@�/@��j@�(�@��P@��#@�p�@�X@�G�@�?}@�7L@��@��`@��D@��m@�l�@�
=@�v�@�V@�M�@�5?@�@���@�x�@�X@�?}@���@��@�j@�A�@�1@�t�@�+@�o@���@��@��y@��!@�ff@�$�@��h@���@��9@�j@�1@��F@�S�@��@�~�@�$�@��T@���@���@��`@�1'@�1@��;@��@�|�@�l�@�
=@��R@���@�~�@��@�@��@��@�r�@��;@��@�+@���@�n�@��^@���@� �@�|�@�S�@�K�@�\)@�+@���@�{@�X@�/@��@�%@���@��9@�j@��@�S�@�"�@�o@��@���@���@�ff@�J@��#@���@���@���@���@�%@��`@��u@�A�@��
@��P@�dZ@�l�@�dZ@�"�@��H@���@��R@��!@���@�v�@��#@��7@�7L@��`@���@��@��@�C�@�
=@�@��H@���@�V@{ƨ@tZ@lz�@d��@XA�@O��@G
=@>��@6$�@/�w@(��@"~�@��@�\@ȴ@dZ@  @�D@��11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   ;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oBC�BC�BC�BC�BC�BC�BC�BC�BD�BE�BF�BK�BXBjB}�B�PB��B��B��B�B�B��BbB�B�B�B5?Bv�B�7B�{B�B^5B\)B`BBy�B�JB��B��B�!B�3B�qBĜBɺB��B��B��B��B��B�)B�NB�ZB�ZB�yB�yB�yB�yB�sB�sB�mB�fB�`B�HB�/B�B��BȴB�jB�3B�B��B��B��B��B�{B�+B�Br�BffB\)BVBM�B9XB,B�B%B�`B��B�^B�bBp�B[#B9XB+B�)B�3B�hB�B� By�Br�BhsBP�B33B$�BB
�sB
�fB
�TB
�;B
��B
�?B
��B
�\B
�B
v�B
n�B
k�B
_;B
N�B
F�B
@�B
:^B
33B
(�B
�B
VB
	7B
  B	��B	��B	��B	�B	��B	�B	�TB	�/B	�
B	��B	��B	�wB	�B	��B	�bB	�1B	|�B	t�B	k�B	^5B	[#B	T�B	E�B	;dB	33B	-B	�B��B�`B�/B�;B�)B�#B�
B��B��BɺBǮBŢBǮBB�wB�^B�qB�qB�jB�jB�^B�RB�?B�?B�9B�9B�3B�3B�9B�?B�?B�FB�FB�LB�LB�LB�jB�wB�wB�wB�wB�qB�dB�dB�^B�XB�LB�XB�^B�jB�dB�dB�dB�dB�^B�dB�^B�}BÖBŢBǮBȴB��B��B��B��B��B��B��B�B�B�`B�`B�`B�sB�B�B�B�yB�yB�B�B�B�B�B�B�B��B��B��B��B��B��B��B��B��B	DB	uB	�B	�B	�B	�B	�B	�B	�B	�B	 �B	!�B	"�B	"�B	"�B	#�B	$�B	+B	5?B	6FB	7LB	<jB	@�B	D�B	H�B	K�B	N�B	P�B	P�B	Q�B	T�B	XB	[#B	[#B	[#B	ZB	ZB	ZB	[#B	[#B	[#B	[#B	ZB	ZB	YB	XB	VB	S�B	R�B	Q�B	P�B	O�B	N�B	M�B	K�B	J�B	J�B	L�B	N�B	P�B	P�B	P�B	P�B	Q�B	Q�B	Q�B	S�B	XB	ZB	_;B	`BB	`BB	bNB	cTB	dZB	e`B	ffB	gmB	gmB	gmB	gmB	ffB	gmB	l�B	n�B	t�B	v�B	w�B	|�B	� B	�B	�B	�B	�7B	�=B	�DB	�JB	�PB	�\B	�bB	�uB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�!B	�'B	�3B	�3B	�3B	�3B	�?B	�?B	�FB	�FB	�FB	�RB	�XB	�XB	�^B	�jB	�}B	��B	��B	��B	��B	��B	B	B	ÖB	ŢB	ȴB	ɺB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�#B	�#B	�)B	�/B	�/B	�5B	�;B	�;B	�BB	�NB	�NB	�ZB	�`B	�`B	�fB	�fB	�fB	�fB	�`B	�ZB	�TB	�HB	�HB	�HB	�HB	�HB	�HB	�BB	�;B	�BB	�BB	�HB	�HB	�BB	�BB	�BB	�BB	�HB	�TB	�TB	�TB	�TB	�ZB	�`B	�mB	�sB	�yB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
B

=B
{B
�B
'�B
.B
6FB
=qB
E�B
K�B
Q�B
XB
_;B
cTB
gmB
jB
m�B
q�B
t�B
w�B
z�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   BC�BC�BC�BC�BC�BC�BC�BC�BD�BE�BF�BK�BXBj�B}�B�TB��B��B��B�B�B��BgB�B�B�B5FBv�B�AB��B�B^<B\2B`GBy�B�QB��B��B�*B�<B�yBĤB��B��B��B��B�B� B�6B�WB�fB�fB�B�B�B�B�B�B�|B�pB�iB�TB�8B�!B��BȾB�uB�<B�B��B��B��B��B��B�3B�Br�BfnB\/BVBM�B9aB,B�B-B�hB��B�eB�hBp�B[)B9\B/B�+B�6B�lB�B�By�Br�BhxBP�B38B$�BB
�xB
�kB
�\B
�AB
��B
�FB
��B
�gB
�B
v�B
n�B
k�B
_DB
N�B
F�B
@�B
:iB
3AB
)B
�B
cB
	CB
 B	�B	� B	��B	��B	��B	�B	�`B	�<B	�B	� B	��B	��B	�B	��B	�sB	�@B	} B	t�B	k�B	^FB	[6B	UB	E�B	;wB	3GB	-!B	�B�B�vB�EB�SB�AB�9B�"B�B��B��B��BŹB��B©B��B�xB��B��B��B��B�uB�kB�XB�XB�SB�QB�LB�IB�RB�ZB�VB�aB�^B�cB�cB�cB��B��B��B��B��B��B�zB�|B�vB�oB�cB�pB�vB��B�{B�zB�yB�{B�vB�~B�uB��BíBŻB��B��B��B��B��B��B�B�B�B�&B�3B�wB�uB�wB�B�B�B�B�B�B�B�B�B��B�B�B�B��B��B��B��B�B�B�B�B�B	WB	�B	�B	�B	�B	�B	�B	�B	�B	�B	 �B	!�B	"�B	"�B	"�B	#�B	$�B	+B	5SB	6YB	7_B	<}B	@�B	D�B	H�B	K�B	N�B	P�B	P�B	Q�B	UB	X B	[4B	[4B	[5B	Z.B	Z/B	Z0B	[5B	[5B	[4B	[1B	Z0B	Z0B	Y)B	X B	VB	T
B	SB	Q�B	P�B	O�B	N�B	M�B	K�B	J�B	J�B	L�B	N�B	P�B	P�B	P�B	P�B	Q�B	Q�B	Q�B	TB	X!B	Z/B	_KB	`SB	`TB	b^B	ceB	dkB	eoB	fvB	g~B	g}B	gB	g|B	fwB	g}B	l�B	n�B	t�B	v�B	w�B	} B	�B	�B	�B	�(B	�FB	�MB	�UB	�ZB	�`B	�hB	�tB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�#B	�0B	�5B	�BB	�CB	�CB	�AB	�NB	�MB	�UB	�SB	�SB	�`B	�dB	�gB	�kB	�yB	��B	��B	��B	��B	��B	��B	B	B	ãB	ůB	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�B	�!B	�0B	�1B	�5B	�9B	�<B	�EB	�HB	�GB	�PB	�YB	�\B	�hB	�nB	�lB	�rB	�sB	�pB	�tB	�nB	�gB	�_B	�UB	�RB	�UB	�UB	�UB	�VB	�OB	�HB	�PB	�PB	�UB	�QB	�PB	�NB	�NB	�OB	�RB	�aB	�`B	�aB	�`B	�eB	�mB	�zB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
B

IB
�B
�B
'�B
. B
6RB
={B
E�B
K�B
Q�B
XB
_GB
c^B
gwB
j�B
m�B
q�B
t�B
w�B
z�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   <#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
PRES            TEMP            PSAL            PRES_ADJUSTED = PRES - dP                                                                                                                                                                                                                                       none                                                                                                                                                                                                                                                            PSAL_ADJUSTED = sw_salt( sw_cndr(PSAL,TEMP,PRES), TEMP, PRES_ADJUSTED )                                                                                                                                                                                         dP =0.04 dbar.                                                                                                                                                                                                                                                  none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            Pressures adjusted by using pressure offset at the sea surface. The quoted error is manufacturer specified accuracy in dbar.                                                                                                                                    The quoted error is manufacturer specified accuracy with respect to ITS-90 at time of laboratory calibration.                                                                                                                                                   No significant salinity drift detected. Salinity adjusted for effects of pressure adjustment. The quoted error is max[0.01, 1xOW uncertainty] in PSS-78.                                                                                                        201605311214382016053112143820160531121438  AO  ARCAADJP                                                                    20140721230548    IP                G�O�G�O�G�O�                AO  ARGQQCPL                                                                    20140721230548  QCP$                G�O�G�O�G�O�DFB7E           AO  ARGQQCPL                                                                    20140721230548  QCF$                G�O�G�O�G�O�0               UW  ARSQUWQC    WOD & nearby Argo as visual check                               20160531121438  IP                  G�O�G�O�G�O�                