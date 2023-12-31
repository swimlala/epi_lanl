CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       N2014-07-21T23:05:14Z AOML 3.0 creation; 2016-05-31T19:14:27Z UW 3.1 conversion     
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
_FillValue                    ��   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  ��   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  0  �|   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                    ��   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                    ��   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                    ��   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  ,  ��   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                    ��   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                    ��   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                    ��   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                    ��   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  @  ��   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                    �(   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                    �8   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    �<   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar         �L   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar         �P   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�        �T   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    �XArgo profile    3.1 1.2 19500101000000  20140721230514  20160531121427  5903736 US ARGO PROJECT                                                 STEPHEN RISER                                                   PRES            TEMP            PSAL               A   AO  4051_7090_016                   2C  D   APEX                            5368                            041511                          846 @�Zv���1   @�Zx!/�@4��vȴ�dK"��`B1   GPS     Primary sampling: mixed [deeper than nominal 985dbar: discrete; nominal 985dbar to surface: 2dbar-bin averaged]                                                                                                                                                    A   A   A   @9��@�  @���A   A   A@  A`  A�  A�  A�  A�  A�  A�  A�  A�  B   B  B  B  B   B(  B0  B8  B@  BH  BP  BX  B`  Bh  Bp  Bx  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  C   C  C  C  C  C
  C  C  C  C  C  C  C  C  C  C  C   C"  C$�C&  C(  C)�fC,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP�CR  CT�CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D��Dy�D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dt� Dt� Dy��D��D�@ D��3D��fD�� D�C3D�y�D���D�3D�33D�p D���D�3D�FfD�ffD�� D�3D�S3D� D��31111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 @7
>@}p�@��@��RA\)A?\)A_\)A\)A��A��A��A��AϮA߮A�A��B�
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
B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��C��C��C��C��C	��C��C��C��C��C��C��C��C��C��C��C��C!��C$]C%��C'��C)�)C+��C-��C/��C1��C3��C5��C7��C9��C;��C=��C?��CA��CC��CE��CG��CI��CK��CM��CP]CQ��CT]CU��CW��CY��C[��C]��C_��Ca��Cc��Ce��Cg��Ci��Ck��Cm��Co��Cq��Cs��Cu��Cw��Cy��C{��C}��C��C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C��C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���D }qD �qD}qD�qD}qD�DwD�qD}qD�qD}qD�qD}qD�qD}qD�qD}qD�qD	}qD	�qD
}qD
�qD}qD�qD}qD�qD}qD�qD}qD�qD}qD�qD}qD�qD}qD�qD}qD�qD}qD�qD}qD�qD}qD�qD}qD�qD}qD�qD}qD�qD}qD�qD}qD�qD}qD�qD}qD�qD}qD�qD}qD�qD}qD�qD }qD �qD!}qD!�qD"}qD"�qD#}qD#�qD$}qD$�qD%}qD%�qD&}qD&�qD'}qD'�qD(}qD(�qD)}qD)�qD*}qD*�qD+}qD+�qD,}qD,�qD-}qD-�qD.}qD.�qD/}qD/�qD0}qD0�qD1}qD1�qD2}qD2�qD3}qD3�qD4}qD4�qD5}qD5�qD6}qD6�qD7}qD7�qD8}qD8�qD9}qD9�qD:}qD:�qD;}qD;�qD<}qD<�qD=}qD=�qD>}qD>�qD?}qD?�qD@}qD@�qDA}qDA�qDB}qDB�qDC}qDC�qDD}qDD�qDE}qDE�qDF}qDF�qDG}qDG�qDH}qDH�qDI}qDI�qDJ}qDJ�qDK}qDK�qDL}qDL�qDM}qDM�qDN}qDN�qDO}qDO�qDP}qDP�qDQ}qDQ�qDR}qDR�qDS}qDS�qDT}qDT�qDU}qDU�qDV}qDV�qDW}qDW�qDX}qDX�qDY}qDY�qDZ}qDZ�qD[}qD[�qD\}qD\�qD]}qD]�qD^}qD^�qD_}qD_�qD`}qD`�qDa}qDa�qDb}qDb�qDc}qDc�qDd}qDd�qDe}qDe�qDf}qDf�qDg}qDg�qDh}qDh�qDi}qDi�qDj}qDj�qDk}qDk�qDl}qDl�qDm}qDm�qDn}qDn�qDo}qDo�qDp}qDp�qDq}qDq�qDr}qDr�qDs}qDs�qDt}qDt�qDy�>D��D�>�D���D��D��D�A�D�xRD�˅D��D�1�D�n�D�˅D��D�ED�eD�θD��D�Q�D�~�D���1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 @��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��A�1'A�1'A�-A�$�A��A� �A�"�A� �A��A��A�A�ĜAԩ�AԍPAԇ+AԁA�~�A�~�A�z�A�r�A�r�A�M�A�9XA�&�A�A��;A���Aљ�A�5?A�  A��HAϼjAϕ�A�K�A��A�7LA�S�Aə�A���A���A�=qA�A�ƨA�A���Aď\A��A��#A�;dA��jA�`BA�33A��A�dZA�dZA�^5A�VA��/A�l�A�C�A�9XA�bNA�A�ffA�{A���A���A�A���A���A�jA��^A�p�A�/A�\)A��TA�+A��A�ƨA���A�%A���A�33A�hsA���A�M�A��TA�dZA��A��A���A��;A�v�A���A�bA��A���A�p�A�  A���A��A���A��uA�E�A��wA��A�|�A�=qA�M�A��jA�33A�l�A�E�A�ZA|��Ax��At�`ArM�Ap��AoO�Aj�`Ahz�Ae�hAdȴAd^5AdJAc�hAbJA`�jA_�FA_A^JA\�`AX�/AWS�AV�!AU�^AT=qAS�AR�HARVAOAM��AM
=AL��AKXAJ1'AH�HAG��AF�\AEG�AC�AC/AB5?AAXA@�9A@ffA?�-A>^5A=%A<�A9`BA7�-A3�mA2VA/��A.5?A-C�A,��A+VA)C�A&�A$Q�A"bA!C�A �A��A�!AQ�A�AC�AQ�AhsA�DA�A"�A��A�AI�A�-A��AK�A�RA��AA��A^5AO�A�A�PA/AA1'A�^A�yAM�A�;Ax�AK�A
=A	`BA�\A�A�A�
At�A?}A�uA��A�AE�A�
A�7A ȴ@�|�@��/@�;d@�$�@��@�1'@�K�@�^5@�@�b@��`@��@�F@�n�@�@�7@��`@�V@�b@��#@�b@���@�@�5?@�v�@��H@���@�|�@և+@թ�@��@�j@��
@�ff@Ѳ-@���@�I�@ϥ�@�"�@ΰ!@�v�@�{@́@���@�Z@� �@�\)@�I�@�K�@�~�@�z�@�33@�p�@�ƨ@�$�@�hs@���@�S�@���@�A�@��;@�
=@���@��@�p�@�&�@�  @�Ĝ@�j@��H@���@���@��!@�5?@��7@�`B@�p�@���@�@��7@��@��7@�x�@�X@�O�@���@�1@�1@�dZ@��\@�E�@�=q@�ff@�J@�G�@���@��u@�I�@��m@�ƨ@�t�@�o@��!@�^5@��@�/@���@�Ĝ@��@��@��@��
@�|�@�S�@�;d@�33@�@���@�M�@�@�x�@�X@�?}@��@���@��/@��@��u@�I�@�ƨ@�|�@�|�@�;d@��y@�~�@�{@��T@��-@�`B@���@��9@�z�@�Q�@��@���@�|�@�33@��@���@��!@��\@�V@���@��@�O�@�&�@���@���@��D@�Z@�Q�@���@�+@��@���@�ff@�-@�{@���@��@�7L@���@�bN@���@��@��;@��;@���@�\)@�C�@�"�@��@���@��@��#@���@�&�@���@��`@�Ĝ@���@�r�@�9X@��@��
@���@�dZ@�o@��@���@��R@��\@�V@�J@��^@���@�x�@�7L@�%@��@��9@�Z@�(�@���@���@�S�@�S�@�S�@�C�@�"�@���@�v�@�M�@�@��T@��^@�O�@��@��/@���@���@�9X@�b@��
@�l�@�"�@�
=@���@���@�@�@���@���@���@���@���@�~�@�ff@�M�@��@���@���@���@��T@��T@���@���@�`B@��@���@��`@���@�z�@�A�@� �@��;@��#@�^5@{��@s��@k��@^�y@S�m@J~�@Dz�@=�@8�`@3��@-�@(1'@"�@ff@n�@?}@Ĝ@�@�`1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 A�1'A�1'A�-A�$�A��A� �A�"�A� �A��A��A�A�ĜAԩ�AԍPAԇ+AԁA�~�A�~�A�z�A�r�A�r�A�M�A�9XA�&�A�A��;A���Aљ�A�5?A�  A��HAϼjAϕ�A�K�A��A�7LA�S�Aə�A���A���A�=qA�A�ƨA�A���Aď\A��A��#A�;dA��jA�`BA�33A��A�dZA�dZA�^5A�VA��/A�l�A�C�A�9XA�bNA�A�ffA�{A���A���A�A���A���A�jA��^A�p�A�/A�\)A��TA�+A��A�ƨA���A�%A���A�33A�hsA���A�M�A��TA�dZA��A��A���A��;A�v�A���A�bA��A���A�p�A�  A���A��A���A��uA�E�A��wA��A�|�A�=qA�M�A��jA�33A�l�A�E�A�ZA|��Ax��At�`ArM�Ap��AoO�Aj�`Ahz�Ae�hAdȴAd^5AdJAc�hAbJA`�jA_�FA_A^JA\�`AX�/AWS�AV�!AU�^AT=qAS�AR�HARVAOAM��AM
=AL��AKXAJ1'AH�HAG��AF�\AEG�AC�AC/AB5?AAXA@�9A@ffA?�-A>^5A=%A<�A9`BA7�-A3�mA2VA/��A.5?A-C�A,��A+VA)C�A&�A$Q�A"bA!C�A �A��A�!AQ�A�AC�AQ�AhsA�DA�A"�A��A�AI�A�-A��AK�A�RA��AA��A^5AO�A�A�PA/AA1'A�^A�yAM�A�;Ax�AK�A
=A	`BA�\A�A�A�
At�A?}A�uA��A�AE�A�
A�7A ȴ@�|�@��/@�;d@�$�@��@�1'@�K�@�^5@�@�b@��`@��@�F@�n�@�@�7@��`@�V@�b@��#@�b@���@�@�5?@�v�@��H@���@�|�@և+@թ�@��@�j@��
@�ff@Ѳ-@���@�I�@ϥ�@�"�@ΰ!@�v�@�{@́@���@�Z@� �@�\)@�I�@�K�@�~�@�z�@�33@�p�@�ƨ@�$�@�hs@���@�S�@���@�A�@��;@�
=@���@��@�p�@�&�@�  @�Ĝ@�j@��H@���@���@��!@�5?@��7@�`B@�p�@���@�@��7@��@��7@�x�@�X@�O�@���@�1@�1@�dZ@��\@�E�@�=q@�ff@�J@�G�@���@��u@�I�@��m@�ƨ@�t�@�o@��!@�^5@��@�/@���@�Ĝ@��@��@��@��
@�|�@�S�@�;d@�33@�@���@�M�@�@�x�@�X@�?}@��@���@��/@��@��u@�I�@�ƨ@�|�@�|�@�;d@��y@�~�@�{@��T@��-@�`B@���@��9@�z�@�Q�@��@���@�|�@�33@��@���@��!@��\@�V@���@��@�O�@�&�@���@���@��D@�Z@�Q�@���@�+@��@���@�ff@�-@�{@���@��@�7L@���@�bN@���@��@��;@��;@���@�\)@�C�@�"�@��@���@��@��#@���@�&�@���@��`@�Ĝ@���@�r�@�9X@��@��
@���@�dZ@�o@��@���@��R@��\@�V@�J@��^@���@�x�@�7L@�%@��@��9@�Z@�(�@���@���@�S�@�S�@�S�@�C�@�"�@���@�v�@�M�@�@��T@��^@�O�@��@��/@���@���@�9X@�b@��
@�l�@�"�@�
=@���@���@�@�@���@���@���@���@���@�~�@�ff@�M�@��@���@���@���@��T@��T@���@���@�`B@��@���@��`@���@�z�@�A�@� �@��;@��#@�^5@{��@s��@k��@^�y@S�m@J~�@Dz�@=�@8�`@3��@-�@(1'@"�@ff@n�@?}@Ĝ@�@�`1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 ;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oBG�BF�BH�BH�BH�BH�BG�BH�BH�BG�BF�BE�BE�BC�BC�BB�BB�BB�BB�BA�BA�B>wB=qB<jB;dB9XB:^B>wBE�BE�BD�BC�BA�B#�BI�B`BBR�BN�BQ�B_;BgmBgmBk�B}�B�bB�hB�JB�PB�=B�7B�\B�PB�PB�uB��B�B��Bz�Bu�Bz�B�B�PB��B}�Bz�B}�B�B�hB��B|�Bq�BcTB\)BZB[#BR�BE�B>wB<jB<jB9XB9XB.B+B�NB�?B�bB�%Bq�B^5BXBp�B�Bm�BgmBe`B_;BB�B$�B
=B
��B
��B
�B
�B
�sB
�;B
�B
�
B
��B
B
�RB
��B
~�B
o�B
Q�B
6FB
�B
DB	��B	�B	�B	ǮB	�XB	�?B	�3B	�'B	�B	��B	��B	��B	�bB	�=B	�B	p�B	jB	gmB	bNB	]/B	YB	VB	Q�B	G�B	?}B	<jB	9XB	49B	.B	(�B	#�B	�B	�B	uB	\B	JB		7B	%B	B	B��B��B��B�B�mB�)B��B��BǮBŢBB�wB�RB�B��B��B��B��B�\B�DB�=B�=B�7B�%B�B�B~�B}�B|�B{�B{�Bz�By�Bx�Bw�Bu�Bt�Bs�Bq�Bn�Bl�Bl�Bk�BjBjBhsBgmBffBe`Be`BdZBbNBaHB_;B^5B\)B[#BZBYBXBW
BW
BW
BW
BVBVBT�BT�BR�BR�BS�BT�BVBVBP�BN�BL�BK�BK�BL�BM�BL�BN�BO�BL�BK�BK�BM�BN�BW
BhsBs�Bu�Bt�Bs�Bs�Bu�Bz�B{�B|�B~�B�B�B�B�%B�+B�+B�1B�=B�DB�JB�DB�=B�PB�PB�JB�DB�7B�%B�B�B� B� B�B�B�B�%B�DB�PB�oB�{B��B��B�B�-B�'B�-B�LB�jB�jB�wB��BĜBȴB��B��B��B�)B�BB�`B�mB�B�B��B��B��B��B��B	B	B	%B		7B	oB	{B	�B	�B	�B	�B	�B	"�B	&�B	-B	-B	.B	/B	49B	8RB	:^B	;dB	<jB	?}B	A�B	C�B	D�B	I�B	M�B	O�B	Q�B	S�B	VB	XB	ZB	]/B	_;B	aHB	e`B	iyB	iyB	k�B	m�B	o�B	r�B	s�B	t�B	v�B	x�B	z�B	|�B	~�B	� B	�B	�B	�+B	�7B	�=B	�DB	�JB	�VB	�\B	�hB	�uB	�{B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�'B	�-B	�3B	�?B	�LB	�XB	�dB	�jB	�qB	�qB	��B	��B	��B	B	ĜB	ƨB	ȴB	ɺB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�
B	�B	�B	�B	�B	�B	�)B	�/B	�5B	�;B	�BB	�BB	�HB	�HB	�TB	�`B	�fB	�fB	�sB	�sB	�sB	�yB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
  B
B
B
B
B
B
B
B
B
B
B
%B
+B
+B
1B
1B
1B
	7B
JB
{B
�B
!�B
(�B
0!B
8RB
?}B
B�B
F�B
M�B
R�B
ZB
`BB
dZB
ffB
jB
n�B
r�B
w�B
{�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 BG�BF�BH�BH�BH�BH�BG�BH�BH�BG�BF�BE�BE�BC�BC�BB�BB�BB�BB�BA�BA�B>|B=sB<nB;gB9[B:`B>BE�BE�BD�BC�BA�B#�BI�B`GBR�BN�BQ�B_ABgsBgsBk�B}�B�fB�kB�NB�VB�@B�8B�bB�UB�TB�zB��B�B��Bz�Bu�Bz�B�B�VB��B}�Bz�B}�B�B�kB��B|�Bq�BcVB\+BZB[)BR�BE�B>|B<oB<nB9ZB9ZB.B/B�PB�BB�dB�)Bq�B^7BXBp�B�Bm�BgqBecB_ABB�B$�B
AB
��B
��B
�B
�B
�wB
�@B
�$B
�B
��B
B
�\B
��B
B
o�B
Q�B
6PB
�B
OB	�B	��B	�#B	ǽB	�fB	�KB	�EB	�6B	�"B	��B	��B	��B	�rB	�MB	�B	p�B	j�B	g�B	b^B	]?B	Y+B	VB	Q�B	G�B	?�B	<{B	9iB	4KB	.'B	)B	#�B	�B	�B	�B	qB	]B		KB	:B	/B	B��B��B��B�B�B�@B�B��B��BŷB§B��B�kB�&B��B��B��B��B�zB�^B�WB�VB�QB�>B�4B�!BB~B}
B|B|Bz�By�Bx�Bw�Bu�Bt�Bs�Bq�Bn�Bl�Bl�Bk�Bj�Bj�Bh�Bg�Bf�Be}Be}BdtBbiBadB_YB^PB\DB[ABZ9BY4BX,BW&BW'BW&BW(BV!BV"BUBUBSBSBTBUBVBV!BQBN�BL�BK�BK�BL�BM�BL�BN�BO�BL�BK�BK�BM�BN�BW&Bh�Bs�Bu�Bt�Bs�Bs�Bu�Bz�B|B}	BB�'B�2B�7B�=B�FB�FB�MB�WB�_B�bB�\B�UB�hB�iB�dB�]B�RB�@B�2B�"B�B�B�'B�(B�.B�>B�_B�jB��B��B��B��B�B�EB�<B�CB�cB��B��B��B��BĳB��B��B��B�B�@B�[B�vB�B�B�B��B��B�	B�B�B	B	 B	8B		KB	�B	�B	�B	�B	�B	�B	�B	"�B	&�B	-B	- B	.(B	/.B	4IB	8cB	:nB	;vB	<}B	?�B	A�B	C�B	D�B	I�B	M�B	O�B	Q�B	TB	VB	X B	Z-B	]@B	_MB	aXB	eoB	i�B	i�B	k�B	m�B	o�B	r�B	s�B	t�B	v�B	x�B	z�B	|�B	
B	�B	�#B	�-B	�9B	�EB	�OB	�RB	�ZB	�fB	�iB	�vB	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�+B	�)B	�6B	�:B	�BB	�LB	�[B	�fB	�pB	�zB	��B	�}B	��B	��B	��B	B	ĩB	ƵB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	� B	�B	�
B	�B	�B	�"B	�#B	�)B	�*B	�5B	�<B	�CB	�GB	�NB	�OB	�RB	�UB	�`B	�kB	�sB	�pB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B
 B
B
B
B
B
B
B
$B
*B
(B
)B
2B
:B
8B
=B
>B
;B
	BB
VB
�B
�B
!�B
) B
0+B
8]B
?�B
B�B
F�B
M�B
R�B
Z&B
`KB
daB
foB
j�B
n�B
r�B
w�B
{�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 <#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
PRES            TEMP            PSAL            PRES_ADJUSTED = PRES - dP                                                                                                                                                                                                                                       none                                                                                                                                                                                                                                                            PSAL_ADJUSTED = sw_salt( sw_cndr(PSAL,TEMP,PRES), TEMP, PRES_ADJUSTED )                                                                                                                                                                                         dP =0.04 dbar.                                                                                                                                                                                                                                                  none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            Pressures adjusted by using pressure offset at the sea surface. The quoted error is manufacturer specified accuracy in dbar.                                                                                                                                    The quoted error is manufacturer specified accuracy with respect to ITS-90 at time of laboratory calibration.                                                                                                                                                   No significant salinity drift detected. Salinity adjusted for effects of pressure adjustment. The quoted error is max[0.01, 1xOW uncertainty] in PSS-78.                                                                                                        201605311214272016053112142720160531121427  AO  ARCAADJP                                                                    20140721230514    IP                G�O�G�O�G�O�                AO  ARGQQCPL                                                                    20140721230514  QCP$                G�O�G�O�G�O�DFB7E           AO  ARGQQCPL                                                                    20140721230514  QCF$                G�O�G�O�G�O�0               UW  ARSQUWQC    WOD & nearby Argo as visual check                               20160531121427  IP                  G�O�G�O�G�O�                