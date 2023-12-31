CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       N2014-10-20T22:00:25Z AOML 3.0 creation; 2016-05-31T19:14:39Z UW 3.1 conversion     
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
_FillValue                    �4Argo profile    3.1 1.2 19500101000000  20141020220025  20160531121439  5903736 US ARGO PROJECT                                                 STEPHEN RISER                                                   PRES            TEMP            PSAL               \A   AO  4051_7090_092                   2C  D   APEX                            5368                            041511                          846 @���_�1   @�J��@4��"���dz��`A�1   GPS     Primary sampling: mixed [deeper than nominal 985dbar: discrete; nominal 985dbar to surface: 2dbar-bin averaged]                                                                                                                                                    \A   A   A   @�33@�  A   A   A@  A`  A�  A�  A�  A�  A�  A�  A�33A�  B ffB  B  B  B   B(  B0  B8  B@  BH  BP  BX  B`ffBg��Bp  Bx  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�33B�33B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  C   C  C  C  C  C
  C  C  C  C  C  C  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*�fD+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� DmfDm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dt� Dt�3Dy� D� D�C3D���D��fD�fD�6fD��fD�ٚD���D�,�D���D�ٚD�	�D�C3Dڐ D���D��D�9�D�y�D���111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  @��@��R@��RA\)A?\)A_\)A\)A��A��A��A��AϮA��GA�B =pB�
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
B`=pBgp�Bo�
Bw�
B�
B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��C��C��C��C��C	��C��C��C��C��C��C��C��C��C��C��C��C!��C#��C%��C'��C)��C+��C-��C/��C1��C3��C5��C7��C9��C;��C=��C?��CA��CC��CE��CG��CI��CK��CM��CO��CQ��CS��CU��CW��CY��C[��C]��C_��Ca��Cc��Ce��Cg��Ci��Ck��Cm��Co��Cq��Cs��Cu��Cw��Cy��C{��C}��C��C���C���C���C���C���C���C���C��C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C��C��C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C��C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���D }qD �qD}qD�qD}qD�qD}qD�qD}qD�qD}qD�qD}qD�qD}qD�qD}qD�qD	}qD	�qD
}qD
�qD}qD�qD}qD�qD}qD�qD}qD�qD}qD�qD}qD�qD}qD�qD}qD�qD}qD�qD}qD�qD}qD�qD}qD�qD}qD�qD}qD�qD}qD�qD}qD�qD}qD�qD}qD�qD}qD�qD}qD�qD}qD�qD }qD �qD!}qD!�qD"}qD"�qD#}qD#�qD$}qD$�qD%}qD%�qD&}qD&�qD'}qD'�qD(}qD(�qD)}qD)�qD*��D*�qD+}qD+�qD,}qD,�qD-}qD-�qD.}qD.�qD/}qD/�qD0}qD0�qD1}qD1�qD2}qD2�qD3}qD3�qD4}qD4�qD5}qD5�qD6}qD6�qD7}qD7�qD8}qD8�qD9}qD9�qD:}qD:�qD;}qD;�qD<}qD<�qD=}qD=�qD>}qD>�qD?}qD?�qD@}qD@�qDA}qDA�qDB}qDB�qDC}qDC�qDD}qDD�qDE}qDE�qDF}qDF�qDG}qDG�qDH}qDH�qDI}qDI�qDJ}qDJ�qDK}qDK�qDL}qDL�qDM}qDM�qDN}qDN�qDO}qDO�qDP}qDP�qDQ}qDQ�qDR}qDR�qDS}qDS�qDT}qDT�qDU}qDU�qDV}qDV�qDW}qDW�qDX}qDX�qDY}qDY�qDZ}qDZ�qD[}qD[�qD\}qD\�qD]}qD]�qD^}qD^�qD_}qD_�qD`}qD`�qDa}qDa�qDb}qDb�qDc}qDc�qDd}qDd�qDe}qDe�qDf}qDf�qDg}qDg�qDh}qDh�qDi}qDi�qDj}qDj�qDk}qDk�qDl}qDm�Dm}qDm�qDn}qDn�qDo}qDo�qDp}qDp�qDq}qDq�qDr}qDr�qDs}qDs�qDt}qDtФDy}qD��D�A�D���D��D�D�5D��D��RD��RD�+�D��RD��RD�RD�A�Dڎ�D�ۅD��D�8RD�xRD��R111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  @��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��A�A�A�-A�FA�^A�RA�jA�jA���A��/A��yA��A�A�(�A�7LA�5?A�&�A�&�A�-A�&�A��A�JA��yA�uA�S�A�ƨA���Aߥ�A�t�Aۏ\A��A�M�A�M�AլA��A���A�z�A��/A�ĜA��/Aɲ-A���A���A�|�A�ĜA�=qA�p�A���A�/A�G�A���A�n�A�ĜA�A�ZA��A�Q�A���A��PA�/A�\)A��-A��yA���A�C�A��A���A�n�A�1'A�bA��TA���A�v�A���A�A�
=A�ȴA�ffA�7LA���A��A�ƨA��A�ZA�E�A��\A�-A��RA��A��hA���A��A��;A�/A��A��jA�XA�VA��TA���A��-A���A�x�A�1A��A���A��A���A�bNA�ĜA���A��A��DA~^5Az��Ay�Ay�Ax  AsG�ApbNAm��Am"�AkXAh�Ag�Ae�AeK�Ad�HAd9XAb�Ab{AaO�AaC�A`�\A_;dA^1A]�A[�#A[�AZ��AY7LAXVAV��AU�-AT��AS�AR��ARM�AR9XAQ�FAQ��AQ��AQ�AOAN�AL�AJ�yAI`BAH=qAFn�ABv�A=��A:jA8�9A7x�A5��A3�#A1&�A/��A/�wA/�wA/�A-;dA*ȴA*ffA*{A)�mA)��A(��A'�;A&M�A%�PA#?}A!��A ��A�!A�wA�AjAS�AVA��AVAE�A��A�jA��AXA�A5?AdZA�A1'A  A��A�!At�AZA/A
VA	�^A	�PA	;dAn�A�AJAhsA��Az�A��A;dA�DA�A/A ��A �j@�
=@�bN@�ff@���@�$�@�7L@�@�P@�-@�9X@�"�@�5?@��@���@�33@���@�/@�w@�n�@�h@�@�A�@��@ݙ�@�Ĝ@�r�@ۅ@�ȴ@�5?@ش9@�C�@�E�@��T@�x�@���@�"�@�5?@��#@�&�@�Q�@�C�@�^5@���@��@���@�@ʟ�@�=q@���@Ɂ@�/@�I�@ǝ�@�n�@ŉ7@�Ĝ@ă@�Q�@�9X@þw@�|�@�"�@¸R@�E�@���@� �@���@�
=@�ff@�5?@���@�7L@��9@�I�@�9X@��
@�@��\@���@�G�@��@���@�9X@��;@�S�@�K�@��y@�n�@�/@���@�9X@���@�+@��@�@���@��7@�x�@�V@���@���@�Q�@���@�
=@���@���@�$�@���@�G�@��@��@��@�Z@�  @���@�t�@�
=@���@���@���@���@�/@��@�Ĝ@�Z@��w@�;d@��@��@���@�E�@�J@��^@���@�x�@�hs@�V@�Ĝ@�bN@��@�\)@�"�@���@�ȴ@��\@�5?@��@�@��7@�X@�&�@��@��@�I�@�  @��@�33@��@�ff@�=q@��@���@��#@�O�@�G�@���@�J@�^5@�v�@�~�@��-@�bN@�1'@�1'@��@��y@�n�@�n�@�{@��7@�&�@���@���@�A�@���@���@��F@���@�dZ@�33@�o@�o@�
=@���@��@���@�M�@��@�{@��@�@���@�hs@�%@���@�r�@��;@��@�;d@��@���@���@�
=@�
=@���@���@�M�@�E�@�=q@���@�`B@��@���@���@���@��@�Z@��@���@�l�@�"�@��!@�n�@�-@��@���@�G�@��@��j@�bN@���@��P@���@���@���@�~�@�n�@�M�@�-@��@��T@��T@��#@�@���@�x�@�hs@�?}@�%@���@��@���@��9@���@���@}��@u/@l�D@c��@\j@UO�@K�m@E�h@@�@;ƨ@4�@.ff@';d@!hs@�T@�u@1@ �@p�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  A�A�A�-A�FA�^A�RA�jA�jA���A��/A��yA��A�A�(�A�7LA�5?A�&�A�&�A�-A�&�A��A�JA��yA�uA�S�A�ƨA���Aߥ�A�t�Aۏ\A��A�M�A�M�AլA��A���A�z�A��/A�ĜA��/Aɲ-A���A���A�|�A�ĜA�=qA�p�A���A�/A�G�A���A�n�A�ĜA�A�ZA��A�Q�A���A��PA�/A�\)A��-A��yA���A�C�A��A���A�n�A�1'A�bA��TA���A�v�A���A�A�
=A�ȴA�ffA�7LA���A��A�ƨA��A�ZA�E�A��\A�-A��RA��A��hA���A��A��;A�/A��A��jA�XA�VA��TA���A��-A���A�x�A�1A��A���A��A���A�bNA�ĜA���A��A��DA~^5Az��Ay�Ay�Ax  AsG�ApbNAm��Am"�AkXAh�Ag�Ae�AeK�Ad�HAd9XAb�Ab{AaO�AaC�A`�\A_;dA^1A]�A[�#A[�AZ��AY7LAXVAV��AU�-AT��AS�AR��ARM�AR9XAQ�FAQ��AQ��AQ�AOAN�AL�AJ�yAI`BAH=qAFn�ABv�A=��A:jA8�9A7x�A5��A3�#A1&�A/��A/�wA/�wA/�A-;dA*ȴA*ffA*{A)�mA)��A(��A'�;A&M�A%�PA#?}A!��A ��A�!A�wA�AjAS�AVA��AVAE�A��A�jA��AXA�A5?AdZA�A1'A  A��A�!At�AZA/A
VA	�^A	�PA	;dAn�A�AJAhsA��Az�A��A;dA�DA�A/A ��A �j@�
=@�bN@�ff@���@�$�@�7L@�@�P@�-@�9X@�"�@�5?@��@���@�33@���@�/@�w@�n�@�h@�@�A�@��@ݙ�@�Ĝ@�r�@ۅ@�ȴ@�5?@ش9@�C�@�E�@��T@�x�@���@�"�@�5?@��#@�&�@�Q�@�C�@�^5@���@��@���@�@ʟ�@�=q@���@Ɂ@�/@�I�@ǝ�@�n�@ŉ7@�Ĝ@ă@�Q�@�9X@þw@�|�@�"�@¸R@�E�@���@� �@���@�
=@�ff@�5?@���@�7L@��9@�I�@�9X@��
@�@��\@���@�G�@��@���@�9X@��;@�S�@�K�@��y@�n�@�/@���@�9X@���@�+@��@�@���@��7@�x�@�V@���@���@�Q�@���@�
=@���@���@�$�@���@�G�@��@��@��@�Z@�  @���@�t�@�
=@���@���@���@���@�/@��@�Ĝ@�Z@��w@�;d@��@��@���@�E�@�J@��^@���@�x�@�hs@�V@�Ĝ@�bN@��@�\)@�"�@���@�ȴ@��\@�5?@��@�@��7@�X@�&�@��@��@�I�@�  @��@�33@��@�ff@�=q@��@���@��#@�O�@�G�@���@�J@�^5@�v�@�~�@��-@�bN@�1'@�1'@��@��y@�n�@�n�@�{@��7@�&�@���@���@�A�@���@���@��F@���@�dZ@�33@�o@�o@�
=@���@��@���@�M�@��@�{@��@�@���@�hs@�%@���@�r�@��;@��@�;d@��@���@���@�
=@�
=@���@���@�M�@�E�@�=q@���@�`B@��@���@���@���@��@�Z@��@���@�l�@�"�@��!@�n�@�-@��@���@�G�@��@��j@�bN@���@��P@���@���@���@�~�@�n�@�M�@�-@��@��T@��T@��#@�@���@�x�@�hs@�?}@�%@���@��@���@��9@���@���@}��@u/@l�D@c��@\j@UO�@K�m@E�h@@�@;ƨ@4�@.ff@';d@!hs@�T@�u@1@ �@p�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  ;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oB<jB<jB<jB=qB?}B>wB>wB?}BH�BQ�BZB_;Bt�B��B�RBǮB�)B�B  BBB+B
=BB  B��B�B�/B��B��B��B��B�5B�TB�BBbB&�B,B;dBB�BE�BQ�B_;Br�Bt�Bs�B�+B��B��B��B��B��B�{B�oB�bB�JB�B|�Br�Bl�BffB^5BQ�BB�B/B!�B{BhBVB
=B��B�sB�B��B��B�B��B��B��B�VBw�B]/B>wB,B�B�BDB��B�B�NB�
B�9B��B�oB|�BgmBQ�B@�B2-B%�B-B33B33B#�B�B	7B
��B
�HB
ƨB
�RB
��B
�PB
s�B
XB
N�B
I�B
<jB
�B
B	�B	�B	�;B	��B	ǮB	�}B	�jB	�XB	�?B	�B	��B	��B	��B	��B	��B	�bB	�DB	�%B	�B	}�B	u�B	p�B	iyB	cTB	^5B	[#B	W
B	T�B	T�B	R�B	Q�B	Q�B	M�B	E�B	>wB	49B	-B	$�B	�B	\B��B�/B��B��BȴBĜB�}B�^B�RB�LB�FB�3B�!B�B��B��B��B��B��B��B��B��B��B�{B�hB�VB�PB�JB�DB�=B�1B�+B�%B�B�B�B}�B|�B{�Bz�By�By�Bx�Bx�Bv�Bv�Bu�Bs�Bs�Bu�B}�B}�B|�B{�Bw�Br�Bt�Bw�Bv�Bt�Br�Bp�Br�Bs�Br�Bq�Bq�Bp�Bq�Bv�Bu�Bu�By�B|�B� B�B�B�1B�7B�7B�7B�+B� B�B�B�B�B�B�DB�DB�PB�VB�VB�VB�VB�bB�hB�oB�oB�uB�{B��B��B��B��B��B�B�B�!B�'B�9B�9B�9B�FB�LB�XBĜBɺB��B��B��B��B�
B�
B�B�B�B�B�B�B�#B�/B�;B�NB�`B�fB�sB�B�B�B�B�B��B��B��B��B	  B	B	B	+B	DB	VB	oB	uB	�B	�B	�B	�B	!�B	"�B	(�B	+B	,B	,B	/B	0!B	0!B	33B	8RB	<jB	=qB	>wB	A�B	E�B	G�B	H�B	I�B	J�B	L�B	O�B	R�B	S�B	W
B	YB	ZB	^5B	aHB	dZB	e`B	ffB	hsB	l�B	p�B	q�B	s�B	w�B	y�B	z�B	|�B	|�B	}�B	}�B	�B	�B	�B	�=B	�DB	�PB	�VB	�\B	�bB	�uB	�{B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�9B	�LB	�^B	�}B	ŢB	ǮB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�)B	�/B	�5B	�;B	�BB	�HB	�ZB	�fB	�yB	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
  B
  B
B
B
B
B
B
B
B
B
B
%B
%B
%B
+B
+B
+B
+B

=B
JB
JB
JB
PB
PB
VB
VB
VB
VB
\B
\B
bB
hB
uB
�B
�B
%�B
,B
2-B
7LB
<jB
B�B
G�B
L�B
O�B
VB
\)B
aHB
e`B
iyB
m�B
q�B
u�B
x�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  B<nB<nB<nB=uB?�B>yB>{B?�BH�BQ�BZ!B_@Bt�B��B�YBǵB�-B�B B$B"B1B
EB"B B��B�~B�7B��B��B��B��B�9B�YB�B#BfB&�B,B;hBB�BE�BQ�B_BBr�Bt�Bs�B�4B��B��B��B��B��B��B�yB�iB�UB�'B|�Br�Bl�BfkB^>BQ�BB�B/!B!�B�BlB]B
EB��B�xB�"B��B��B�B��B��B��B�ZBw�B]5B>{B,B�B�BEB��B�B�QB�B�<B��B�rB|�BgsBQ�B@�B23B%�B-B38B38B#�B�B	;B
��B
�PB
ưB
�[B
��B
�ZB
s�B
XB
N�B
I�B
<sB
�B
+B	�B	�B	�FB	��B	ǺB	��B	�wB	�fB	�KB	�"B	��B	��B	��B	��B	��B	�rB	�UB	�5B	�B	~B	u�B	p�B	i�B	chB	^EB	[2B	WB	UB	UB	SB	Q�B	Q�B	M�B	E�B	>�B	4LB	-"B	$�B	�B	qB��B�EB�B��B��BĴB��B�wB�jB�dB�_B�JB�<B�B�B�B�
B��B��B��B��B��B��B��B��B�qB�kB�fB�_B�XB�KB�EB�?B�;B�4B�!B~B}B|Bz�By�By�Bx�Bx�Bv�Bv�Bu�Bs�Bs�Bu�B~B~B}B|Bw�Br�Bt�Bw�Bv�Bt�Br�Bp�Br�Bs�Br�Bq�Bq�Bp�Bq�Bv�Bu�Bu�By�B}B�B�%B�9B�MB�PB�SB�PB�EB�B�(B�!B�"B�"B�9B�]B�]B�jB�oB�nB�oB�mB�{B��B��B��B��B��B��B��B� B�B�B�B�2B�:B�?B�QB�QB�NB�^B�dB�pBĳB��B��B��B�B�B�!B�"B�(B�/B�0B�,B�.B�3B�6B�FB�OB�dB�tB�}B�B�B�B�B�B��B��B��B��B�B	 B	B	3B	@B	XB	iB	�B	�B	�B	�B	�B	�B	!�B	"�B	)
B	+B	,B	,B	/.B	04B	03B	3EB	8dB	<}B	=�B	>�B	A�B	E�B	G�B	H�B	I�B	J�B	L�B	O�B	SB	T
B	WB	Y(B	Z/B	^GB	aVB	djB	eqB	ftB	h�B	l�B	p�B	q�B	s�B	w�B	y�B	z�B	} B	} B	~B	~B	�B	�"B	�'B	�MB	�TB	�_B	�fB	�kB	�rB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�*B	�GB	�YB	�kB	��B	ŰB	ǻB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	� B	� B	�B	�B	�B	�B	�B	�B	�B	�%B	�+B	�'B	�)B	�)B	�7B	�<B	�DB	�HB	�OB	�VB	�fB	�sB	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B
 B
 B
B
B
B
B
B
 B
'B
%B
&B
1B
0B
0B
9B
8B
8B
:B

GB
WB
UB
UB
[B
ZB
_B
bB
bB
aB
gB
gB
mB
rB
�B
�B
�B
%�B
,B
28B
7XB
<rB
B�B
G�B
L�B
O�B
VB
\3B
aSB
eiB
i�B
m�B
q�B
u�B
x�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  <#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
PRES            TEMP            PSAL            PRES_ADJUSTED = PRES - dP                                                                                                                                                                                                                                       none                                                                                                                                                                                                                                                            PSAL_ADJUSTED = sw_salt( sw_cndr(PSAL,TEMP,PRES), TEMP, PRES_ADJUSTED )                                                                                                                                                                                         dP =0.04 dbar.                                                                                                                                                                                                                                                  none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            Pressures adjusted by using pressure offset at the sea surface. The quoted error is manufacturer specified accuracy in dbar.                                                                                                                                    The quoted error is manufacturer specified accuracy with respect to ITS-90 at time of laboratory calibration.                                                                                                                                                   No significant salinity drift detected. Salinity adjusted for effects of pressure adjustment. The quoted error is max[0.01, 1xOW uncertainty] in PSS-78.                                                                                                        201605311214402016053112144020160531121440  AO  ARCAADJP                                                                    20141020220025    IP                G�O�G�O�G�O�                AO  ARGQQCPL                                                                    20141020220025  QCP$                G�O�G�O�G�O�DFB7E           AO  ARGQQCPL                                                                    20141020220025  QCF$                G�O�G�O�G�O�0               UW  ARSQUWQC    WOD & nearby Argo as visual check                               20160531121440  IP                  G�O�G�O�G�O�                