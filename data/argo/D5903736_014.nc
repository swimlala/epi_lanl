CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       N2014-07-21T23:05:13Z AOML 3.0 creation; 2016-05-31T19:14:26Z UW 3.1 conversion     
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
_FillValue                    �Argo profile    3.1 1.2 19500101000000  20140721230513  20160531121426  5903736 US ARGO PROJECT                                                 STEPHEN RISER                                                   PRES            TEMP            PSAL               A   AO  4051_7090_014                   2C  D   APEX                            5368                            041511                          846 @�UXu `1   @�UYο�@4�+J�d6�1'1   GPS     Primary sampling: mixed [deeper than nominal 985dbar: discrete; nominal 985dbar to surface: 2dbar-bin averaged]                                                                                                                                                    A   A   A   @�33@�  A   A   A@  A`  A�  A�  A�  A�  A�  A�  A�  A�  B   B  B  B  B   B(  B0  B8  B@  BH  BP  BX  B`  BhffBp��Bw��B�  B�  B���B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  C   C  C  C  C  C
  C  C  C  C  C  C  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dty�Dy� D���D�P D���D�ٚD� D�9�D�� D�� D�3D�S3D��fD�� D�3D�P D�vfD��fD�fD�9�D�p D��311111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   @��@��R@��RA\)A?\)A_\)A\)A��A��A��A��AϮA߮A�A��B�
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
Bh=pBp��Bwp�B�
B��B��RB��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��C��C��C��C��C	��C��C��C��C��C��C��C��C��C��C��C��C!��C#��C%��C'��C)��C+��C-��C/��C1��C3��C5��C7��C9��C;��C=��C?��CA��CC��CE��CG��CI��CK��CM��CO��CQ��CS��CU��CW��CY��C[��C]��C_��Ca��Cc��Ce��Cg��Ci��Ck��Cm��Co��Cq��Cs��Cu��Cw��Cy��C{��C}��C��C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���D }qD �qD}qD�qD}qD�qD}qD�qD}qD�qD}qD�qD}qD�qD}qD�qD}qD�qD	}qD	�qD
}qD
�qD}qD�qD}qD�qD}qD�qD}qD�qD}qD�qD}qD�qD}qD�qD}qD�qD}qD�qD}qD�qD}qD�qD}qD�qD}qD�qD}qD�qD}qD�qD}qD�qD}qD�qD}qD�qD}qD�qD}qD�qD}qD�qD }qD �qD!}qD!�qD"}qD"�qD#}qD#�qD$}qD$�qD%}qD%�qD&}qD&�qD'}qD'�qD(}qD(�qD)}qD)�qD*}qD*�qD+}qD+�qD,}qD,�qD-}qD-�qD.}qD.�qD/}qD/�qD0}qD0�qD1}qD1�qD2}qD2�qD3}qD3�qD4}qD4�qD5}qD5�qD6}qD6�qD7}qD7�qD8}qD8�qD9}qD9�qD:}qD:�qD;}qD;�qD<}qD<�qD=}qD=�qD>}qD>�qD?}qD?�qD@}qD@�qDA}qDA�qDB}qDB�qDC}qDC�qDD}qDD�qDE}qDE�qDF}qDF�qDG}qDG�qDH}qDH�qDI}qDI�qDJ}qDJ�qDK}qDK�qDL}qDL�qDM}qDM�qDN}qDN�qDO}qDO�qDP}qDP�qDQ}qDQ�qDR}qDR�qDS}qDS�qDT}qDT�qDU}qDU�qDV}qDV�qDW}qDW�qDX}qDX�qDY}qDY�qDZ}qDZ�qD[}qD[�qD\}qD\�qD]}qD]�qD^}qD^�qD_}qD_�qD`}qD`�qDa}qDa�qDb}qDb�qDc}qDc�qDd}qDd�qDe}qDe�qDf}qDf�qDg}qDg�qDh}qDh�qDi}qDi�qDj}qDj�qDk}qDk�qDl}qDl�qDm}qDm�qDn}qDn�qDo}qDo�qDp}qDp�qDq}qDq�qDr}qDr�qDs}qDs�qDtwDy}qD��D�N�D��RD��RD��D�8RD�~�D���D��D�Q�D��D�θD��D�N�D�uD��D�D�8RD�n�D���11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   @��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��A�+A�/A�7LA�9XA�9XA�7LA�A�A�A�A�C�A�E�A�C�A�A�A�;dA�9XA�A��#A���A���A���A�Aթ�A�I�A�%Aԇ+A�O�A���A�ȴAӝ�A�~�A�v�A҃A���A�{A���A�r�Aȴ9A��HA��A��A�VA�ZA���A�  A�(�A�M�A��7A��yA�?}A���A�hsA�+A���A�^5A��9A���A��#A���A�1A�`BA���A�A�A�A���A�x�A��RA�oA�~�A�A�ƨA�A�ȴA���A��RA��DA�~�A��A�A��HA�bA���A�t�A��TA���A�Q�A��hA���A��PA���A�jA��A��A��A��hA�VA���A�`BA���A���A���A�I�A���A��A��A�bNA��A�&�A��yA�"�A���A�33A�|�A�I�A�ĜA���A�t�A�#A�A}?}A|(�AzĜAw�TAt-Aq�FAoO�AnbAl�Aix�Ag&�AdjA`�A]�#A]�^A]7LA\��A[%AY��AYC�AX��AV�`AT�AP9XAN=qAM+AL�`AL��ALȴAL��AL��ALjAK;dAI��AF��ADJA@�`A=�A<ZA;�-A7�;A5�A5C�A4�+A4bA3C�A1�PA/XA.��A.��A.z�A.M�A-�A,z�A*�`A*E�A)�A%�wA#�PA"VA!�^A!C�A �A �A 9XA��AG�AoA|�A��A+A�9A�+A;dA7LAt�A�^A�A1'AS�A�!A~�AK�A�A��AoA\)A	�7A	/A	%A`BAƨAA�A�A�hA n�@���@�A�@���@���@�Z@�F@��@�dZ@��H@�\@�{@���@�;d@�^5@��`@�t�@�;d@��#@ܛ�@���@�@��;@�o@�ff@�o@�hs@�%@�K�@͡�@���@��m@�I�@�K�@�t�@� �@��
@�S�@�@�I�@�V@��y@�
=@�
=@�~�@�/@���@��/@ě�@ēu@ă@�A�@��@�dZ@�@�@�$�@��-@�hs@�ƨ@��H@�X@�  @���@�K�@���@���@�r�@��F@�t�@�33@��@��@�9X@�K�@��y@��\@�ff@�V@�V@�=q@���@���@��#@�@���@���@��h@��-@��#@���@��@���@�hs@�?}@�7L@���@��@��9@�z�@�j@�Q�@��w@��@��!@�n�@�=q@��@�@���@�@��u@���@�;d@���@�^5@���@�x�@�p�@�?}@��/@��u@��@�Z@�1@��
@��P@�dZ@�;d@�"�@���@�-@��@��^@���@��h@�/@��D@�b@��m@��@�C�@���@��+@�5?@�$�@�J@��-@�O�@�%@��`@��j@�I�@��;@���@���@��@�K�@���@�v�@�M�@�-@��@��#@��h@�O�@�7L@�V@���@�bN@�Z@�I�@�b@��w@��@�C�@���@��\@�E�@��@���@��7@��@�O�@���@���@��D@�1'@�  @�ƨ@�\)@�+@��@�ȴ@���@�V@�{@��^@�G�@��@�%@���@���@�bN@�9X@� �@��@�33@�
=@��H@��@��!@�v�@�ff@�$�@��^@��-@���@���@��j@��@�(�@��@��F@�\)@���@�~�@�^5@�=q@�-@�J@���@��h@�p�@�O�@�%@���@��D@��@�I�@�I�@�A�@�A�@�I�@�Z@�A�@�I�@��@�Q�@� �@��;@���@�33@�@��@��H@���@��!@�~�@�^5@�5?@�O�@�V@�V@��j@��@��@��D@�Q�@�9X@� �@��@���@�C�@���@��j@{33@p��@j�!@`�u@Y7L@QX@Lz�@D�@=`B@8�u@3�@-O�@%@!�#@p�@  @��@r�@��@�;11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   A�+A�/A�7LA�9XA�9XA�7LA�A�A�A�A�C�A�E�A�C�A�A�A�;dA�9XA�A��#A���A���A���A�Aթ�A�I�A�%Aԇ+A�O�A���A�ȴAӝ�A�~�A�v�A҃A���A�{A���A�r�Aȴ9A��HA��A��A�VA�ZA���A�  A�(�A�M�A��7A��yA�?}A���A�hsA�+A���A�^5A��9A���A��#A���A�1A�`BA���A�A�A�A���A�x�A��RA�oA�~�A�A�ƨA�A�ȴA���A��RA��DA�~�A��A�A��HA�bA���A�t�A��TA���A�Q�A��hA���A��PA���A�jA��A��A��A��hA�VA���A�`BA���A���A���A�I�A���A��A��A�bNA��A�&�A��yA�"�A���A�33A�|�A�I�A�ĜA���A�t�A�#A�A}?}A|(�AzĜAw�TAt-Aq�FAoO�AnbAl�Aix�Ag&�AdjA`�A]�#A]�^A]7LA\��A[%AY��AYC�AX��AV�`AT�AP9XAN=qAM+AL�`AL��ALȴAL��AL��ALjAK;dAI��AF��ADJA@�`A=�A<ZA;�-A7�;A5�A5C�A4�+A4bA3C�A1�PA/XA.��A.��A.z�A.M�A-�A,z�A*�`A*E�A)�A%�wA#�PA"VA!�^A!C�A �A �A 9XA��AG�AoA|�A��A+A�9A�+A;dA7LAt�A�^A�A1'AS�A�!A~�AK�A�A��AoA\)A	�7A	/A	%A`BAƨAA�A�A�hA n�@���@�A�@���@���@�Z@�F@��@�dZ@��H@�\@�{@���@�;d@�^5@��`@�t�@�;d@��#@ܛ�@���@�@��;@�o@�ff@�o@�hs@�%@�K�@͡�@���@��m@�I�@�K�@�t�@� �@��
@�S�@�@�I�@�V@��y@�
=@�
=@�~�@�/@���@��/@ě�@ēu@ă@�A�@��@�dZ@�@�@�$�@��-@�hs@�ƨ@��H@�X@�  @���@�K�@���@���@�r�@��F@�t�@�33@��@��@�9X@�K�@��y@��\@�ff@�V@�V@�=q@���@���@��#@�@���@���@��h@��-@��#@���@��@���@�hs@�?}@�7L@���@��@��9@�z�@�j@�Q�@��w@��@��!@�n�@�=q@��@�@���@�@��u@���@�;d@���@�^5@���@�x�@�p�@�?}@��/@��u@��@�Z@�1@��
@��P@�dZ@�;d@�"�@���@�-@��@��^@���@��h@�/@��D@�b@��m@��@�C�@���@��+@�5?@�$�@�J@��-@�O�@�%@��`@��j@�I�@��;@���@���@��@�K�@���@�v�@�M�@�-@��@��#@��h@�O�@�7L@�V@���@�bN@�Z@�I�@�b@��w@��@�C�@���@��\@�E�@��@���@��7@��@�O�@���@���@��D@�1'@�  @�ƨ@�\)@�+@��@�ȴ@���@�V@�{@��^@�G�@��@�%@���@���@�bN@�9X@� �@��@�33@�
=@��H@��@��!@�v�@�ff@�$�@��^@��-@���@���@��j@��@�(�@��@��F@�\)@���@�~�@�^5@�=q@�-@�J@���@��h@�p�@�O�@�%@���@��D@��@�I�@�I�@�A�@�A�@�I�@�Z@�A�@�I�@��@�Q�@� �@��;@���@�33@�@��@��H@���@��!@�~�@�^5@�5?@�O�@�V@�V@��j@��@��@��D@�Q�@�9X@� �@��@���@�C�@���@��j@{33@p��@j�!@`�u@Y7L@QX@Lz�@D�@=`B@8�u@3�@-O�@%@!�#@p�@  @��@r�@��@�;11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   ;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oB�B�B�B�B�B�B�B�B�B�B�B�B�B�B~�B}�B}�B~�B�B�+B�hB�FB��B��BB	7BoB�B!�B"�B/B=qBW
Bq�Bt�Bm�BaHBN�B[#BYBZBYBZB[#BZBXBVBS�BQ�BO�BM�BJ�BE�B>wB5?B.B'�B�B�B�BbB	7B��B��B��B  B��B�B�B�)BĜB�B�JBt�BaHBYBK�B<jB5?B,B�BB��B�HBɺB�qB�-B��B�JBv�Bp�BgmB_;BM�B8RB1'B,B%�B�BB
�B
�`B
�B
��B
ǮB
�dB
�LB
�B
��B
��B
�uB
�B
y�B
u�B
q�B
iyB
dZB
W
B
N�B
C�B
.B
�B
B	��B	�B	�5B	ɺB	�?B	��B	�B	v�B	v�B	v�B	t�B	jB	`BB	YB	R�B	L�B	A�B	6FB	+B	'�B	'�B	'�B	(�B	,B	5?B	5?B	1'B	)�B	�B	bB	B�B�B�fB�;B�B��B��B��B��BÖB�jB�dB�^B�^B�^B�RB�FB�?B�'B��B��B��B��B��B��B��B��B�{B�uB�hB�\B�JB�7B�+B�%B�B� B{�Bx�Bw�Bu�Bt�Br�Bq�Bo�Bm�Bl�Bk�BiyBk�Bt�Bu�Bu�Br�Bp�Bs�Bs�Bn�BhsBgmBiyBiyBhsBe`B`BB\)BZB]/BffBl�Bl�Bm�Bn�Bn�Bn�Bn�Bm�BgmB[#BXBVBQ�BN�BL�BI�BI�BM�BJ�BF�BO�B[#BdZBiyBe`B[#B\)B]/B`BBu�B�PB��B��B��B��B��B��B�B�B�!B�!B�-B�9B�LB�RB�XB�XB�^B�dB�wB�wB�dB�LB�FB�9B�3B�-B�-B�'B�'B�!B�B�-B�9B�RB�dB�qB�}B�}B�}B��BŢB��B��B��B��B�
B�B�/B�;B�NB�mB�yB�B��B��B	B	B	%B	
=B	DB	DB	\B	�B	�B	�B	�B	�B	�B	�B	�B	#�B	&�B	-B	0!B	49B	8RB	;dB	;dB	=qB	@�B	B�B	B�B	C�B	F�B	G�B	I�B	J�B	K�B	L�B	N�B	S�B	W
B	ZB	[#B	\)B	^5B	cTB	e`B	e`B	gmB	iyB	l�B	n�B	p�B	q�B	q�B	s�B	v�B	x�B	y�B	z�B	}�B	�B	�+B	�1B	�+B	�7B	�JB	�PB	�\B	�bB	�hB	�hB	�uB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�'B	�9B	�FB	�XB	�RB	�RB	�XB	�^B	�qB	�wB	�}B	��B	ÖB	ĜB	ĜB	ŢB	ƨB	ǮB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�
B	�
B	�B	�B	�B	�B	�#B	�/B	�/B	�/B	�BB	�HB	�NB	�ZB	�`B	�fB	�sB	�yB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B
  B
B
B
B
B
B
B
B
B
B
B
B
B
B
%B
+B
+B
+B
+B
1B
1B
1B
	7B
	7B

=B

=B
DB
hB
�B
!�B
(�B
2-B
6FB
=qB
=qB
D�B
J�B
O�B
T�B
YB
_;B
bNB
ffB
k�B
o�B
r�B
u�B
{�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   B�B�B�B�B�B�B�B�B�B�B�B�B�B�B~�B}�B}�B B�B�0B�mB�NB��B��BB	9BsB�B!�B"�B/!B=zBWBq�Bt�Bm�BaOBN�B[*BY BZ&BYBZ&B[*BZ$BXBVBT BQ�BO�BM�BJ�BE�B>}B5GB.B'�B�B�B�BeB	:B��B��B��B B��B�B�B�-BĥB�B�RBt�BaLBYBK�B<nB5DB,B�B&B��B�JBɽB�uB�2B��B�NBv�Bp�BgqB_@BM�B8VB1-B,B%�B�BB
�B
�hB
�
B
��B
ǲB
�kB
�SB
�B
��B
��B
�}B
�!B
y�B
u�B
q�B
i�B
dbB
WB
N�B
C�B
.B
�B
+B	��B	�B	�BB	��B	�MB	��B	�.B	v�B	v�B	v�B	t�B	j�B	`RB	Y*B	SB	L�B	A�B	6YB	+B	(B	(B	(B	)B	,B	5SB	5QB	18B	*B	�B	uB	B��B�B�~B�QB�)B�B�B��B��BîB��B�|B�wB�uB�wB�hB�_B�[B�>B�B��B��B��B��B��B��B��B��B��B��B�xB�eB�QB�FB�=B�2B�B|Bx�Bw�Bu�Bt�Br�Bq�Bo�Bm�Bl�Bk�Bi�Bk�Bt�Bu�Bu�Br�Bp�Bs�Bs�Bn�Bh�Bg�Bi�Bi�Bh�Be|B``B\EBZ9B]JBf�Bl�Bl�Bm�Bn�Bn�Bn�Bn�Bm�Bg�B[>BX+BV BRBN�BL�BI�BI�BM�BJ�BF�BO�B[=BdrBi�BezB[>B\FB]JB`ZBu�B�hB��B��B��B��B�B�B�B�-B�9B�9B�DB�RB�dB�gB�nB�pB�tB�{B��B��B�zB�cB�^B�OB�LB�BB�DB�=B�>B�7B�4B�EB�RB�jB�|B��B��B��B��B��BŹB��B��B��B�B� B�3B�CB�PB�bB�B�B�B��B��B	B	%B	:B	
PB	WB	XB	pB	�B	�B	�B	�B	�B	�B	�B	�B	#�B	&�B	-B	03B	4JB	8eB	;tB	;uB	=�B	@�B	B�B	B�B	C�B	F�B	G�B	I�B	J�B	K�B	L�B	N�B	TB	WB	Z/B	[3B	\:B	^EB	ceB	epB	eoB	gB	i�B	l�B	n�B	p�B	q�B	q�B	s�B	v�B	x�B	y�B	z�B	~B	�B	�:B	�AB	�;B	�DB	�YB	�`B	�mB	�sB	�wB	�vB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�(B	�6B	�HB	�SB	�dB	�aB	�_B	�eB	�jB	�~B	��B	��B	��B	äB	īB	ĩB	ůB	ƶB	ǼB	��B	��B	��B	��B	��B	��B	��B	�B	��B	��B	�B	�B	�B	�B	�B	�"B	�+B	�.B	�:B	�;B	�?B	�OB	�VB	�ZB	�fB	�mB	�tB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B
 B
B
B
B
B
 B
 B
!B
B
B
 B
B
B
B
1B
7B
8B
8B
8B
=B
?B
>B
	AB
	BB

IB

IB
PB
pB
�B
!�B
(�B
29B
6PB
=|B
={B
D�B
J�B
O�B
UB
Y!B
_CB
bWB
fpB
k�B
o�B
r�B
u�B
{�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   <#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
PRES            TEMP            PSAL            PRES_ADJUSTED = PRES - dP                                                                                                                                                                                                                                       none                                                                                                                                                                                                                                                            PSAL_ADJUSTED = sw_salt( sw_cndr(PSAL,TEMP,PRES), TEMP, PRES_ADJUSTED )                                                                                                                                                                                         dP =0.04 dbar.                                                                                                                                                                                                                                                  none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            Pressures adjusted by using pressure offset at the sea surface. The quoted error is manufacturer specified accuracy in dbar.                                                                                                                                    The quoted error is manufacturer specified accuracy with respect to ITS-90 at time of laboratory calibration.                                                                                                                                                   No significant salinity drift detected. Salinity adjusted for effects of pressure adjustment. The quoted error is max[0.01, 1xOW uncertainty] in PSS-78.                                                                                                        201605311214262016053112142620160531121426  AO  ARCAADJP                                                                    20140721230513    IP                G�O�G�O�G�O�                AO  ARGQQCPL                                                                    20140721230513  QCP$                G�O�G�O�G�O�DFB7E           AO  ARGQQCPL                                                                    20140721230513  QCF$                G�O�G�O�G�O�0               UW  ARSQUWQC    WOD & nearby Argo as visual check                               20160531121426  IP                  G�O�G�O�G�O�                