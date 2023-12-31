CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       N2015-04-23T02:15:46Z AOML 3.0 creation; 2016-05-31T19:14:42Z UW 3.1 conversion     
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
_FillValue                    �Argo profile    3.1 1.2 19500101000000  20150423021546  20160531121443  5903736 US ARGO PROJECT                                                 STEPHEN RISER                                                   PRES            TEMP            PSAL               nA   AO  4051_7090_110                   2C  D   APEX                            5368                            041511                          846 @�K0�E��1   @�K1.E @3���R�dV��O�;1   GPS     Primary sampling: mixed [deeper than nominal 985dbar: discrete; nominal 985dbar to surface: 2dbar-bin averaged]                                                                                                                                                    nA   A   A   @���@�  A   A   A@  A`  A�  A�  A�  A�  A�  A�  A�  A�  B   B  B  BffB   B(  B0  B8  B@  BH  BP  BX  B`ffBg��Bp  Bx  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  C   C  C  C  C  C
  C  C  C  C  C  C  C  C  C  C  C   C"  C$  C&�C(  C*  C,  C.�C0  C2�C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� D`��Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dt� Dy�fD��D�I�D�y�D���D� D�0 D�� D�� D��D�C3D���Dǰ D� D�<�Dډ�D��3D���D�FfD�3D�S311111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   @�Q�@��R@��RA\)A?\)A_\)A\)A��A��A��A��AϮA߮A�A��B�
B�
B=pB�
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
B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��C��C��C��C��C	��C��C��C��C��C��C��C��C��C��C��C��C!��C#��C&]C'��C)��C+��C.]C/��C2]C3��C5��C7��C9��C;��C=��C?��CA��CC��CE��CG��CI��CK��CM��CO��CQ��CS��CU��CW��CY��C[��C]��C_��Ca��Cc��Ce��Cg��Ci��Ck��Cm��Co��Cq��Cs��Cu��Cw��Cy��C{��C}��C��C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���D }qD �qD}qD�qD}qD�qD}qD�qD}qD�qD}qD�qD}qD�qD}qD�qD}qD�qD	}qD	�qD
}qD
�qD}qD�qD}qD�qD}qD�qD}qD�qD}qD�qD}qD�qD}qD�qD}qD�qD}qD�qD}qD�qD}qD�qD}qD�qD}qD�qD}qD�qD}qD�qD}qD�qD}qD�qD}qD�qD}qD�qD}qD�qD}qD�qD }qD �qD!}qD!�qD"}qD"�qD#}qD#�qD$}qD$�qD%}qD%�qD&}qD&�qD'}qD'�qD(}qD(�qD)}qD)�qD*}qD*�qD+}qD+�qD,}qD,�qD-}qD-�qD.}qD.�qD/}qD/�qD0}qD0�qD1}qD1�qD2}qD2�qD3}qD3�qD4}qD4�qD5}qD5�qD6}qD6�qD7}qD7�qD8}qD8�qD9}qD9�qD:}qD:�qD;}qD;�qD<}qD<�qD=}qD=�qD>}qD>�qD?}qD?�qD@}qD@�qDA}qDA�qDB}qDB�qDC}qDC�qDD}qDD�qDE}qDE�qDF}qDF�qDG}qDG�qDH}qDH�qDI}qDI�qDJ}qDJ�qDK}qDK�qDL}qDL�qDM}qDM�qDN}qDN�qDO}qDO�qDP}qDP�qDQ}qDQ�qDR}qDR�qDS}qDS�qDT}qDT�qDU}qDU�qDV}qDV�qDW}qDW�qDX}qDX�qDY}qDY�qDZ}qDZ�qD[}qD[�qD\}qD\�qD]}qD]�qD^}qD^�qD_}qD_�qD`}qD`�Da}qDa�qDb}qDb�qDc}qDc�qDd}qDd�qDe}qDe�qDf}qDf�qDg}qDg�qDh}qDh�qDi}qDi�qDj}qDj�qDk}qDk�qDl}qDl�qDm}qDm�qDn}qDn�qDo}qDo�qDp}qDp�qDq}qDq�qDr}qDr�qDs}qDs�qDt}qDy��D��D�HRD�xRD�˅D��D�.�D�~�D���D�RD�A�D��RDǮ�D��D�;�DڈRD���D���D�ED��D�Q�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   @��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��A�A�A�A�A�A�A�%A�%A�%A�1A�1A�1A�1A�1A�
=A�JA�JA�VA�VA�VA�VA�bA�oAΥ�A�n�A���A��HA�5?A�p�A���A��A�C�AĴ9A�$�A�
=A��#A�A�A��A�bNA���A�E�A��`A�ffA��yA�ȴA�XA��RA�K�A��A��jA���A�t�A��FA�n�A�&�A��9A�
=A�C�A���A�
=A�x�A��!A�"�A�=qA��A�?}A��-A�bNA��A���A��\A�A�hsA�|�A��`A��7A��A�n�A�7LA��FA�A�A��A���A��+A�5?A��A�%A���A�-A��A�  A�%A��\A�=qA��/A��A�Q�A�JA��/A�dZA��A��A�O�A���A��#A��/A��7A�?}A��TA�`BA�I�A��yA�^5A�ƨA��hA��yA��A�n�A��A�A���A��9A�(�A��jA�7LA�$�Ax�As`BAo�7Am/Ai�
Ai%Ah��AhAg��Af�HAfVAf  Ae/AbM�A_ƨA]�7A[�AV��AU�AS�PAQ��AO��AM�^AK�
AI|�AHZAG�TAG�ADv�AB��ABM�A@�9A=�;A:bNA8�A8{A7��A4r�A3?}A3+A3/A3�A2�`A2n�A1�A1\)A0�A0 �A//A.1'A-hsA,�A+��A*1A'A%�PA$��A$A�A#"�A!��A �+A
=A�A��A��A�A�A�AVA��A�7A�wA�+A5?A�A��A�+A-AffA��Ar�A
�uA	��A	hsAĜA �A�FA��A-A\)AZA��A�AE�A��A/A �u@���@��@��@��/@�\)@��m@���@���@�@�bN@�@�9@�I�@��;@�~�@���@웦@�t�@�bN@�dZ@���@�$�@���@� �@�dZ@◍@�J@�X@��@�bN@��@���@�Q�@�1@���@���@��@�p�@��;@ҸR@�Ĝ@�r�@ϝ�@ΰ!@��@���@���@ʇ+@�p�@��y@�^5@�?}@�Z@�l�@�o@§�@�$�@�$�@��@��D@�ƨ@�K�@�ff@�$�@�$�@�^5@��@��R@�=q@��T@�?}@�Ĝ@�Ĝ@��@�Z@�K�@��@�v�@��h@��@�I�@�ƨ@���@���@�t�@�C�@���@��R@�5?@���@�G�@��@���@��P@���@��@�33@��@�?}@�Ĝ@�I�@��P@�33@���@�ff@�@��T@��-@��7@��@�O�@���@��@�j@�1'@��;@�l�@�;d@�
=@��y@���@��\@�M�@�@���@�?}@��`@�Q�@�1'@���@���@�t�@��R@���@�Ĝ@� �@��P@�t�@�t�@��w@��F@�l�@�;d@���@�{@��@�7L@�X@�G�@��`@��9@�A�@�1'@�(�@� �@�  @��@�S�@�ƨ@�C�@�"�@���@�-@�{@���@��@�Ĝ@��9@��D@�r�@�Ĝ@��@�Q�@��w@�ƨ@���@��@�S�@�
=@�@��y@���@�v�@�J@���@��7@�G�@��@��@�j@��@��;@�C�@��y@���@�5?@���@�G�@�&�@��@���@�  @��w@�l�@���@�ff@�^5@�n�@��+@�5?@��#@�@��h@�x�@�O�@�O�@�&�@���@��@��@��`@���@���@��9@��u@�bN@�Z@��@��@��w@�l�@��@��y@��@��+@�~�@�n�@�-@�J@�?}@���@��@�Z@�9X@�(�@�1@���@�33@��@��y@��R@�ff@�@��h@�V@���@�Q�@�A�@�1'@��
@��@�l�@�t�@�l�@�K�@���@��@y�#@p�u@d��@_��@Y�#@PA�@G\)@@�@7�@1&�@,�@'�;@#o@V@~�@�+@33@b@�D11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   A�A�A�A�A�A�A�%A�%A�%A�1A�1A�1A�1A�1A�
=A�JA�JA�VA�VA�VA�VA�bA�oAΥ�A�n�A���A��HA�5?A�p�A���A��A�C�AĴ9A�$�A�
=A��#A�A�A��A�bNA���A�E�A��`A�ffA��yA�ȴA�XA��RA�K�A��A��jA���A�t�A��FA�n�A�&�A��9A�
=A�C�A���A�
=A�x�A��!A�"�A�=qA��A�?}A��-A�bNA��A���A��\A�A�hsA�|�A��`A��7A��A�n�A�7LA��FA�A�A��A���A��+A�5?A��A�%A���A�-A��A�  A�%A��\A�=qA��/A��A�Q�A�JA��/A�dZA��A��A�O�A���A��#A��/A��7A�?}A��TA�`BA�I�A��yA�^5A�ƨA��hA��yA��A�n�A��A�A���A��9A�(�A��jA�7LA�$�Ax�As`BAo�7Am/Ai�
Ai%Ah��AhAg��Af�HAfVAf  Ae/AbM�A_ƨA]�7A[�AV��AU�AS�PAQ��AO��AM�^AK�
AI|�AHZAG�TAG�ADv�AB��ABM�A@�9A=�;A:bNA8�A8{A7��A4r�A3?}A3+A3/A3�A2�`A2n�A1�A1\)A0�A0 �A//A.1'A-hsA,�A+��A*1A'A%�PA$��A$A�A#"�A!��A �+A
=A�A��A��A�A�A�AVA��A�7A�wA�+A5?A�A��A�+A-AffA��Ar�A
�uA	��A	hsAĜA �A�FA��A-A\)AZA��A�AE�A��A/A �u@���@��@��@��/@�\)@��m@���@���@�@�bN@�@�9@�I�@��;@�~�@���@웦@�t�@�bN@�dZ@���@�$�@���@� �@�dZ@◍@�J@�X@��@�bN@��@���@�Q�@�1@���@���@��@�p�@��;@ҸR@�Ĝ@�r�@ϝ�@ΰ!@��@���@���@ʇ+@�p�@��y@�^5@�?}@�Z@�l�@�o@§�@�$�@�$�@��@��D@�ƨ@�K�@�ff@�$�@�$�@�^5@��@��R@�=q@��T@�?}@�Ĝ@�Ĝ@��@�Z@�K�@��@�v�@��h@��@�I�@�ƨ@���@���@�t�@�C�@���@��R@�5?@���@�G�@��@���@��P@���@��@�33@��@�?}@�Ĝ@�I�@��P@�33@���@�ff@�@��T@��-@��7@��@�O�@���@��@�j@�1'@��;@�l�@�;d@�
=@��y@���@��\@�M�@�@���@�?}@��`@�Q�@�1'@���@���@�t�@��R@���@�Ĝ@� �@��P@�t�@�t�@��w@��F@�l�@�;d@���@�{@��@�7L@�X@�G�@��`@��9@�A�@�1'@�(�@� �@�  @��@�S�@�ƨ@�C�@�"�@���@�-@�{@���@��@�Ĝ@��9@��D@�r�@�Ĝ@��@�Q�@��w@�ƨ@���@��@�S�@�
=@�@��y@���@�v�@�J@���@��7@�G�@��@��@�j@��@��;@�C�@��y@���@�5?@���@�G�@�&�@��@���@�  @��w@�l�@���@�ff@�^5@�n�@��+@�5?@��#@�@��h@�x�@�O�@�O�@�&�@���@��@��@��`@���@���@��9@��u@�bN@�Z@��@��@��w@�l�@��@��y@��@��+@�~�@�n�@�-@�J@�?}@���@��@�Z@�9X@�(�@�1@���@�33@��@��y@��R@�ff@�@��h@�V@���@�Q�@�A�@�1'@��
@��@�l�@�t�@�l�@�K�@���@��@y�#@p�u@d��@_��@Y�#@PA�@G\)@@�@7�@1&�@,�@'�;@#o@V@~�@�+@33@b@�D11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   ;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oB
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
��B
��B
��B
��B
��B
�B
�mB�B�BB?}BL�BR�B[#Bk�Bl�Bw�B�VB��B�RB��B�B��B1BhB�B�B/BB�B=qB>wBM�BR�BT�BS�BJ�BG�BG�BP�BT�BM�BH�BD�BG�B<jB49B$�B�B0!B1'BE�BK�BQ�BaHB`BB\)BXBS�BN�BB�B<jBG�B9XB)�B�B�BbBB�B��B�B��B~�B{�B{�BdZBH�BuB��B�B�B�B�B�sB�HB�#B�
BǮB�B��B��B�1Bq�B_;BJ�B?}B33BuB
��B
�B
�B
��B
�dB
��B
�7B
�B
y�B
m�B
H�B
oB	�fB	ǮB	�3B	��B	��B	��B	��B	��B	��B	�hB	�\B	�DB	u�B	dZB	T�B	E�B	6FB	-B	#�B	�B	bB	1B	B��B��B��B�B�sB�NB�;B�B��B��BɺBǮBĜBB��B��B��B�}B�}B�qB�^B�RB�?B�-B�!B�B��B��B��B��B�\B�=B�1B�+B�%B�%B�B� B~�B~�B� B� B� B|�B}�B|�B|�B|�B|�B|�B{�By�Bx�By�By�Bx�Bv�Bv�Bv�Bu�Bu�Bv�Bu�Bv�Bu�Bu�Bu�Bt�Bq�Bm�Bk�BgmBdZBcTBe`Bo�Bp�Bl�BaHBYB]/BjBq�Bu�B{�B}�B|�B{�By�Bv�Bv�Bw�Bv�Bv�Bv�Bw�Bw�Bx�B}�B�B�B�=B�7B�+B�%B�+B�+B�=B�PB�PB�JB�PB�\B�{B��B��B��B��B��B��B��B��B��B��B�B�B�!B�FB�LB�dB�}BBŢBƨBȴB��B��B�B�5B�B�B�B��B��B��B��B��B��B��B��B	+B	1B	+B	%B	%B	+B		7B	
=B	DB	DB	JB	JB	VB	oB	�B	�B	�B	�B	�B	 �B	�B	�B	�B	 �B	%�B	-B	5?B	8RB	<jB	=qB	@�B	B�B	C�B	E�B	J�B	L�B	S�B	[#B	^5B	bNB	e`B	gmB	gmB	gmB	k�B	n�B	s�B	v�B	|�B	�B	�B	�B	�+B	�\B	�hB	�hB	��B	��B	��B	��B	��B	��B	��B	��B	�B	�'B	�'B	�!B	�!B	�-B	�3B	�9B	�?B	�FB	�RB	�RB	�RB	�XB	�XB	�^B	�qB	��B	B	ÖB	B	��B	B	��B	�}B	�wB	�wB	�}B	��B	ŢB	ȴB	ȴB	ǮB	ȴB	ȴB	ȴB	��B	��B	��B	��B	��B	��B	��B	��B	�B	�
B	�
B	�B	�B	�#B	�/B	�BB	�BB	�HB	�HB	�NB	�NB	�TB	�TB	�TB	�ZB	�fB	�mB	�sB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
B
B
B
B
B
B
B
%B
+B
1B
	7B
	7B
	7B

=B

=B
DB
JB
JB
VB
hB
�B
�B
'�B
(�B
49B
9XB
@�B
C�B
K�B
R�B
W
B
\)B
bNB
e`B
iyB
k�B
p�B
t�B
v�B
y�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   B
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
��B
��B
��B
��B
��B
�B
�tB�%B�BB?�BL�BR�B['Bk�Bl�Bw�B�VB��B�ZB��B�B��B8BkB�B�B/!BB�B=wB>BM�BR�BUBS�BJ�BG�BG�BP�BUBM�BH�BD�BG�B<nB4?B$�B�B0&B1,BE�BK�BQ�BaOB`HB\0BXBT BN�BB�B<qBG�B9aB*B�B�BgB!B�B��B�B��B~�B{�B{�Bd^BH�BxB��B�B�B�B�B�wB�LB�&B�BǵB�B��B��B�6Bq�B_ABJ�B?�B38B}B
��B
�B
�B
��B
�jB
��B
�BB
�B
y�B
m�B
H�B
wB	�sB	ǽB	�AB	��B	��B	��B	��B	��B	��B	�wB	�nB	�VB	u�B	dlB	UB	E�B	6XB	-"B	#�B	�B	vB	FB	!B��B��B��B�B�B�dB�QB�-B�
B��B��B��BĶB§B��B��B��B��B��B��B�vB�jB�VB�GB�;B�(B�B�
B��B��B�vB�XB�KB�DB�?B�?B�3B�BBB�B�B�B}B~B}B}	B}	B}
B}	B|By�Bx�By�By�Bx�Bv�Bv�Bv�Bu�Bu�Bv�Bu�Bv�Bu�Bu�Bu�Bt�Bq�Bm�Bk�Bg�BdsBcpBe{Bo�Bp�Bl�BadBY2B]KBj�Bq�Bu�B|B~B}B|By�Bv�Bv�Bw�Bv�Bv�Bv�Bw�Bw�Bx�B~B�8B�1B�WB�RB�DB�@B�EB�DB�YB�gB�jB�eB�kB�uB��B��B��B��B��B��B��B��B��B��B�B�(B�&B�9B�^B�dB�|B��B¤BŹBƾB��B��B�B�&B�KB�B��B��B��B��B�B�B�B�B��B�B	?B	GB	>B	9B	8B	@B		LB	
QB	YB	WB	\B	]B	iB	�B	�B	�B	�B	�B	�B	 �B	�B	�B	�B	 �B	%�B	-"B	5QB	8eB	<|B	=�B	@�B	B�B	C�B	E�B	J�B	L�B	TB	[4B	^GB	b^B	erB	g}B	g|B	g}B	k�B	n�B	s�B	v�B	|�B	�B	�)B	�1B	�:B	�kB	�vB	�wB	��B	��B	��B	��B	��B	��B	��B	��B	�B	�4B	�6B	�1B	�0B	�<B	�AB	�CB	�OB	�SB	�_B	�_B	�_B	�gB	�eB	�mB	�~B	��B	B	âB	B	��B	B	��B	��B	��B	��B	��B	��B	ŮB	��B	ȿB	ǿB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�B	�*B	�0B	�>B	�PB	�RB	�UB	�SB	�]B	�ZB	�aB	�cB	�aB	�gB	�rB	�xB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	� B	� B	�B	�B	�B	�B
B
B
 B
 B
B
&B
-B
2B
7B
=B
	AB
	CB
	DB

IB

JB
NB
VB
UB
`B
sB
�B
�B
'�B
) B
4CB
9dB
@�B
C�B
K�B
R�B
WB
\2B
bXB
ekB
i�B
k�B
p�B
t�B
v�B
y�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   <#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
PRES            TEMP            PSAL            PRES_ADJUSTED = PRES - dP                                                                                                                                                                                                                                       none                                                                                                                                                                                                                                                            PSAL_ADJUSTED = sw_salt( sw_cndr(PSAL,TEMP,PRES), TEMP, PRES_ADJUSTED )                                                                                                                                                                                         dP =0.04 dbar.                                                                                                                                                                                                                                                  none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            Pressures adjusted by using pressure offset at the sea surface. The quoted error is manufacturer specified accuracy in dbar.                                                                                                                                    The quoted error is manufacturer specified accuracy with respect to ITS-90 at time of laboratory calibration.                                                                                                                                                   No significant salinity drift detected. Salinity adjusted for effects of pressure adjustment. The quoted error is max[0.01, 1xOW uncertainty] in PSS-78.                                                                                                        201605311214432016053112144320160531121443  AO  ARCAADJP                                                                    20150423021546    IP                G�O�G�O�G�O�                AO  ARGQQCPL                                                                    20150423021546  QCP$                G�O�G�O�G�O�DFB5E           AO  ARGQQCPL                                                                    20150423021546  QCF$                G�O�G�O�G�O�0               UW  ARSQUWQC    WOD & nearby Argo as visual check                               20160531121443  IP                  G�O�G�O�G�O�                