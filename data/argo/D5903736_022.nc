CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       N2014-07-21T23:05:17Z AOML 3.0 creation; 2016-05-31T19:14:28Z UW 3.1 conversion     
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
_FillValue                    �Argo profile    3.1 1.2 19500101000000  20140721230517  20160531121428  5903736 US ARGO PROJECT                                                 STEPHEN RISER                                                   PRES            TEMP            PSAL               A   AO  4051_7090_022                   2C  D   APEX                            5368                            041511                          846 @�i��)_�1   @�i�[��@3̋C���d�l�C��1   GPS     Primary sampling: mixed [deeper than nominal 985dbar: discrete; nominal 985dbar to surface: 2dbar-bin averaged]                                                                                                                                                    A   A   A   @�  @�  A   A   A@  A`  A�  A�  A�  A�  A�  A�  A�  A�  B   B  B  B  B   B(  B0  B8  B@ffBH  BO33BX  B`  Bh  Bp  Bx  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  C   C  C  C  C  C
  C  C  C  C  C  C  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd�Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D�fD  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dty�Dyl�D�fD�I�D�y�D�ɚD�	�D�<�D�l�D��fD���D�L�D��fD��fD�3D�S3DچfD��fD���D�<�D�l�D��311111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   @}p�@��R@��RA\)A?\)A_\)A\)A��A��A��A��AϮA߮A�A��B�
B�
B�
B�
B'�
B/�
B7�
B@=pBG�
BO
=BW�
B_�
Bg�
Bo�
Bw�
B�
B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��C��C��C��C��C	��C��C��C��C��C��C��C��C��C��C��C��C!��C#��C%��C'��C)��C+��C-��C/��C1��C3��C5��C7��C9��C;��C=��C?��CA��CC��CE��CG��CI��CK��CM��CO��CQ��CS��CU��CW��CY��C[��C]��C_��Ca��Cd]Ce��Cg��Ci��Ck��Cm��Co��Cq��Cs��Cu��Cw��Cy��C{��C}��C��C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C��C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���D }qD �qD}qD�qD}qD�qD}qD�qD}qD�qD}qD�qD}qD�qD}qD�qD}qD�qD	}qD	�qD
}qD
�qD}qD�qD��D�qD}qD�qD}qD�qD}qD�qD}qD�qD}qD�qD}qD�qD}qD�qD}qD�qD}qD�qD}qD�qD}qD�qD}qD�qD}qD�qD}qD�qD}qD�qD}qD�qD}qD�qD}qD�qD}qD�qD }qD �qD!}qD!�qD"}qD"�qD#}qD#�qD$}qD$�qD%}qD%�qD&}qD&�qD'}qD'�qD(}qD(�qD)}qD)�qD*}qD*�qD+}qD+�qD,}qD,�qD-}qD-�qD.}qD.�qD/}qD/�qD0}qD0�qD1}qD1�qD2}qD2�qD3}qD3�qD4}qD4�qD5}qD5�qD6}qD6�qD7}qD7�qD8}qD8�qD9}qD9�qD:}qD:�qD;}qD;�qD<}qD<�qD=}qD=�qD>}qD>�qD?}qD?�qD@}qD@�qDA}qDA�qDB}qDB�qDC}qDC�qDD}qDD�qDE}qDE�qDF}qDF�qDG}qDG�qDH}qDH�qDI}qDI�qDJ}qDJ�qDK}qDK�qDL}qDL�qDM}qDM�qDN}qDN�qDO}qDO�qDP}qDP�qDQ}qDQ�qDR}qDR�qDS}qDS�qDT}qDT�qDU}qDU�qDV}qDV�qDW}qDW�qDX}qDX�qDY}qDY�qDZ}qDZ�qD[}qD[�qD\}qD\�qD]}qD]�qD^}qD^�qD_}qD_�qD`}qD`�qDa}qDa�qDb}qDb�qDc}qDc�qDd}qDd�qDe}qDe�qDf}qDf�qDg}qDg�qDh}qDh�qDi}qDi�qDj}qDj�qDk}qDk�qDl}qDl�qDm}qDm�qDn}qDn�qDo}qDo�qDp}qDp�qDq}qDq�qDr}qDr�qDs}qDs�qDtwDyj>D�D�HRD�xRD��RD�RD�;�D�k�D��D��RD�K�D��D��D��D�Q�DڅD��D��RD�;�D�k�D���11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   @��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��AփAօA֋DA֋DA֋DA֏\A֥�A֬AֶFA־wAּjAֺ^A�A���A���A־wA־wAֺ^AֶFA֬A֟�A�?}A��#A�K�AЃA͑hA�I�A�l�AǮA�XA�-A��A���A�
=A���A�Q�A�-A�jA���A� �A���A���A��A�~�A�9XA�I�A��PA�5?A��jA�;dA��TA�K�A���A�oA���A�x�A��RA��!A�9XA���A��A���A�~�A�dZA���A��FA�=qA�n�A�9XA��FA���A�v�A�5?A���A�&�A�ƨA�/A�|�A�A�A�p�A��A�C�A�r�A�7LA���A��uA�\)A�33A��A��PA��jA�5?A�=qA��yA�\)A��A�l�A�%A��7A� �A��A��wA�E�A�C�A���A��
A�jA��A�  A�`BA��wA~��A{7LAxz�At�jAq�PAm"�Aj�Ah��Ag/Ad=qAa�mAaoA_"�A\=qAXr�AUt�AT��AS�AQƨAPJAM��AK�;AI�AHAF5?AD(�ACABjA@VA>v�A>�+A<��A< �A;?}A9��A8��A7|�A6�A6�A5��A57LA4�jA4{A2�A0�HA/�A.�A-ƨA,z�A, �A+��A+?}A*1A(n�A%�#A#�mA!��A!G�A r�A��AVAA��A��A7LA%A�A�PA��A��A�
A�hA��A�A7LAv�A  A5?A��A�;AƨAt�AK�A�`A��A
�A
JA	��AjA"�A"�An�A�
AA�!AƨA �A z�@�{@�ƨ@��@��F@�@��@�C�@�h@땁@�M�@�j@�ff@�J@��@㕁@��@�-@��@�r�@���@ٲ-@�Ĝ@�I�@��@�M�@�@��@�`B@���@���@Л�@�9X@��;@�K�@�V@́@�p�@�7L@��/@���@�o@��#@Ȭ@��@���@�Z@�|�@��@�`B@ă@öF@��@�&�@���@��D@�r�@�bN@� �@���@��;@���@��w@���@���@�%@�A�@��@��m@���@�"�@�{@�Ĝ@��@��y@��@���@���@��H@��H@�E�@���@�&�@��@�Z@�  @��w@��@�\)@�K�@�K�@�\)@�C�@�ȴ@�v�@�-@�hs@���@��u@��@��@�dZ@�o@���@��R@�^5@�J@�@�`B@��@���@���@�ƨ@�"�@��y@���@��\@�n�@���@��7@��@�p�@�7L@��D@�I�@��@�1@�ƨ@�|�@�K�@�
=@��R@��@���@��@�&�@�j@�  @��F@�\)@�33@�+@��H@�ff@��-@�p�@��@���@�Z@�(�@�  @��;@��P@�+@�@��y@���@���@��R@���@��+@�^5@��@�hs@�X@�G�@�/@���@���@���@�Q�@� �@���@��
@��@�|�@��@�l�@�;d@��y@�^5@��T@���@�`B@�&�@��@�%@���@��/@��9@�I�@���@�|�@�t�@�
=@���@�n�@�$�@��^@��7@�G�@�&�@���@���@�j@�  @�ƨ@���@���@�K�@�+@�"�@���@���@�n�@�M�@��T@���@��7@�`B@�G�@���@�r�@�(�@��
@��@�t�@�S�@�;d@�"�@��@��@�ȴ@���@��R@��\@�V@�E�@�5?@�@��^@���@���@��h@�p�@�X@�&�@��@��u@�1'@��
@��w@�\)@�+@�o@��@��!@���@���@���@�5?@�$�@��@��@�X@�V@��/@��/@���@�Ĝ@���@�r�@�Q�@� �@�1@��;@��w@���@��P@�C�@��D@\)@x��@o
=@c��@Y��@Q�^@M/@E�@A�^@<1@4�/@-O�@&5?@!%@I�@@�!@;d@
��@+11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   AփAօA֋DA֋DA֋DA֏\A֥�A֬AֶFA־wAּjAֺ^A�A���A���A־wA־wAֺ^AֶFA֬A֟�A�?}A��#A�K�AЃA͑hA�I�A�l�AǮA�XA�-A��A���A�
=A���A�Q�A�-A�jA���A� �A���A���A��A�~�A�9XA�I�A��PA�5?A��jA�;dA��TA�K�A���A�oA���A�x�A��RA��!A�9XA���A��A���A�~�A�dZA���A��FA�=qA�n�A�9XA��FA���A�v�A�5?A���A�&�A�ƨA�/A�|�A�A�A�p�A��A�C�A�r�A�7LA���A��uA�\)A�33A��A��PA��jA�5?A�=qA��yA�\)A��A�l�A�%A��7A� �A��A��wA�E�A�C�A���A��
A�jA��A�  A�`BA��wA~��A{7LAxz�At�jAq�PAm"�Aj�Ah��Ag/Ad=qAa�mAaoA_"�A\=qAXr�AUt�AT��AS�AQƨAPJAM��AK�;AI�AHAF5?AD(�ACABjA@VA>v�A>�+A<��A< �A;?}A9��A8��A7|�A6�A6�A5��A57LA4�jA4{A2�A0�HA/�A.�A-ƨA,z�A, �A+��A+?}A*1A(n�A%�#A#�mA!��A!G�A r�A��AVAA��A��A7LA%A�A�PA��A��A�
A�hA��A�A7LAv�A  A5?A��A�;AƨAt�AK�A�`A��A
�A
JA	��AjA"�A"�An�A�
AA�!AƨA �A z�@�{@�ƨ@��@��F@�@��@�C�@�h@땁@�M�@�j@�ff@�J@��@㕁@��@�-@��@�r�@���@ٲ-@�Ĝ@�I�@��@�M�@�@��@�`B@���@���@Л�@�9X@��;@�K�@�V@́@�p�@�7L@��/@���@�o@��#@Ȭ@��@���@�Z@�|�@��@�`B@ă@öF@��@�&�@���@��D@�r�@�bN@� �@���@��;@���@��w@���@���@�%@�A�@��@��m@���@�"�@�{@�Ĝ@��@��y@��@���@���@��H@��H@�E�@���@�&�@��@�Z@�  @��w@��@�\)@�K�@�K�@�\)@�C�@�ȴ@�v�@�-@�hs@���@��u@��@��@�dZ@�o@���@��R@�^5@�J@�@�`B@��@���@���@�ƨ@�"�@��y@���@��\@�n�@���@��7@��@�p�@�7L@��D@�I�@��@�1@�ƨ@�|�@�K�@�
=@��R@��@���@��@�&�@�j@�  @��F@�\)@�33@�+@��H@�ff@��-@�p�@��@���@�Z@�(�@�  @��;@��P@�+@�@��y@���@���@��R@���@��+@�^5@��@�hs@�X@�G�@�/@���@���@���@�Q�@� �@���@��
@��@�|�@��@�l�@�;d@��y@�^5@��T@���@�`B@�&�@��@�%@���@��/@��9@�I�@���@�|�@�t�@�
=@���@�n�@�$�@��^@��7@�G�@�&�@���@���@�j@�  @�ƨ@���@���@�K�@�+@�"�@���@���@�n�@�M�@��T@���@��7@�`B@�G�@���@�r�@�(�@��
@��@�t�@�S�@�;d@�"�@��@��@�ȴ@���@��R@��\@�V@�E�@�5?@�@��^@���@���@��h@�p�@�X@�&�@��@��u@�1'@��
@��w@�\)@�+@�o@��@��!@���@���@���@�5?@�$�@��@��@�X@�V@��/@��/@���@�Ĝ@���@�r�@�Q�@� �@�1@��;@��w@���@��P@�C�@��D@\)@x��@o
=@c��@Y��@Q�^@M/@E�@A�^@<1@4�/@-O�@&5?@!%@I�@@�!@;d@
��@+11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   ;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oBH�BI�BI�BI�BI�BJ�BO�BP�BR�BT�BS�BS�BT�BT�BT�BT�BT�BT�BT�BS�BR�BN�BA�B&�B�B�B{BoBhBbBJBDB	7B+BB1BPB�B�BoB{BhB#�B%�B$�B �B�BhB\BDB	7BBBB  B��B��B�TBȴB�B��B��B��B��B��B��B�!B��B��B��B�PB� Bz�Bp�B`BBH�B+BVB��B�mB��B�}B�'B��B�Bq�Bm�BiyBffB\)BE�B;dB.B&�B�B�BhB
=BB
��B
��B
�mB
��B
��B
�?B
��B
�=B
� B
{�B
r�B
dZB
F�B
2-B
 �B
	7B	�B	�B	ǮB	�jB	�-B	��B	�hB	�=B	|�B	k�B	XB	G�B	C�B	A�B	9XB	49B	&�B	{B��B�B�;B�B��B��BɺBȴB��BɺB��B��B��B��B��B��B��B��B��B��BǮB��B�wB�qB�dB�XB�RB�LB�?B�3B�B��B��B��B��B�{B�oB�hB�\B�VB�JB�=B�+B�B� B|�By�Bz�B{�Bz�Bz�Bz�Bx�Bu�Br�Bk�BiyBiyBjBhsBgmBe`Be`BffBe`BjBk�BhsB`BBZBW
BW
BVBVBVBT�BS�BP�BM�BK�BJ�BG�BI�BK�BI�BI�BK�BR�B\)B_;BcTBdZBbNB^5B`BBcTB_;B]/BT�BT�BW
BYB]/B_;BaHBcTBdZBe`BffBffBiyBs�Bt�Bv�Bv�Bt�Bs�Br�Bv�Bw�By�B� B~�B�B�B�B�B�B�%B�%B�%B�%B�%B�+B�1B�1B�7B�1B�JB�=B�=B�\B�uB��B��B��B��B��B��B��B��B��B��B�9B�XB�jB�}BBÖBȴB��B��B��B��B��B�B�B�)B�NB�mB�yB�B�B�B��B��B��B	  B	  B	B	B	1B	
=B	VB	\B	uB	�B	�B	"�B	$�B	&�B	)�B	+B	2-B	49B	5?B	5?B	7LB	>wB	@�B	B�B	C�B	E�B	H�B	I�B	K�B	M�B	O�B	P�B	Q�B	T�B	ZB	\)B	]/B	_;B	`BB	bNB	dZB	ffB	k�B	l�B	n�B	r�B	t�B	u�B	w�B	w�B	z�B	~�B	� B	� B	�B	�B	�B	�B	�B	�B	�1B	�JB	�PB	�PB	�\B	�hB	�oB	�oB	�{B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�B	�!B	�'B	�9B	�dB	�qB	�wB	��B	B	ĜB	ŢB	ȴB	��B	��B	��B	��B	��B	��B	�
B	�B	�B	�B	�)B	�/B	�/B	�5B	�BB	�HB	�HB	�NB	�TB	�`B	�fB	�mB	�sB	�yB	�yB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
  B
B
B
B
B
B
B
B
B
B
B
%B
%B
+B
+B
+B
1B
1B
1B
1B
1B
	7B
	7B
	7B
	7B

=B

=B
DB
bB
�B
�B
#�B
)�B
0!B
5?B
<jB
B�B
G�B
M�B
S�B
\)B
bNB
ffB
iyB
o�B
r�B
v�B
z�B
~�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   BH�BI�BI�BI�BI�BJ�BO�BP�BR�BUBT BT BUBUBUBUBUBUBUBS�BR�BN�BA�B&�B�B�B�BxBoBiBNBIB	;B2B$B6BVB�B�BwB�BiB#�B%�B$�B �B�BnB`BJB	?B'BBB B��B��B�[BȻB� B��B��B��B��B��B��B�)B��B��B��B�SB�Bz�Bp�B`FBH�B+BWB��B�pB��B��B�)B��B�"Bq�Bm�BizBfjB\.BE�B;gB.B&�B�B�BoB
ABB
��B
��B
�sB
��B
��B
�FB
��B
�GB
�B
{�B
r�B
deB
F�B
24B
 �B
	CB	��B	�B	ǼB	�wB	�<B	��B	�yB	�NB	} B	k�B	X#B	G�B	C�B	A�B	9jB	4LB	&�B	�B�B�B�SB�&B�B��B��B��B��B��B��B��B��B��B��B�	B��B��B��B��B��B��B��B��B�|B�pB�jB�eB�YB�LB�5B�B��B��B��B��B��B��B�xB�oB�eB�VB�FB�-B�B}	By�Bz�B|Bz�Bz�Bz�Bx�Bu�Br�Bk�Bi�Bi�Bj�Bh�Bg�Be|Be}Bf�Be{Bj�Bk�Bh�B`_BZ7BW'BW&BV BV BV BUBTBQBM�BK�BJ�BG�BI�BK�BI�BI�BK�BSB\EB_WBcnBdwBbkB^QB`_BcpB_TB]KBUBUBW'BY2B]GB_WBadBcqBdxBe{Bf�Bf�Bi�Bs�Bt�Bv�Bv�Bt�Bs�Br�Bv�Bw�By�B�BB�"B�!B�"B�-B�.B�@B�@B�>B�<B�?B�CB�KB�KB�QB�JB�eB�VB�VB�vB��B��B��B��B��B��B��B��B��B��B�B�OB�pB��B��B¨BïB��B��B��B��B�B�B�B�,B�@B�dB�B�B�B�B��B��B��B�B	 B	 B	 B	5B	DB	
OB	jB	oB	�B	�B	�B	"�B	$�B	&�B	*B	+B	2AB	4JB	5OB	5RB	7]B	>�B	@�B	B�B	C�B	E�B	H�B	I�B	K�B	M�B	O�B	P�B	Q�B	UB	Z/B	\8B	]@B	_JB	`TB	b\B	dkB	fvB	k�B	l�B	n�B	r�B	t�B	u�B	w�B	w�B	z�B	
B	�B	�B	�B	�B	�B	�#B	�!B	�)B	�@B	�[B	�^B	�bB	�lB	�wB	�|B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�!B	� B	�(B	�/B	�3B	�GB	�sB	��B	��B	��B	B	ĩB	ůB	ȿB	��B	��B	��B	��B	��B	��B	�B	�#B	�,B	�,B	�7B	�:B	�;B	�CB	�NB	�VB	�VB	�[B	�_B	�lB	�sB	�xB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	��B
 B
B
B
B
B
B
B
B
$B
$B
+B
2B
2B
7B
:B
8B
=B
=B
=B
>B
>B
	BB
	CB
	CB
	CB

IB

JB
NB
lB
�B
�B
#�B
*B
0,B
5IB
<sB
B�B
G�B
M�B
T B
\1B
bXB
fpB
i�B
o�B
r�B
v�B
z�B
11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   <#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
PRES            TEMP            PSAL            PRES_ADJUSTED = PRES - dP                                                                                                                                                                                                                                       none                                                                                                                                                                                                                                                            PSAL_ADJUSTED = sw_salt( sw_cndr(PSAL,TEMP,PRES), TEMP, PRES_ADJUSTED )                                                                                                                                                                                         dP =0.04 dbar.                                                                                                                                                                                                                                                  none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            Pressures adjusted by using pressure offset at the sea surface. The quoted error is manufacturer specified accuracy in dbar.                                                                                                                                    The quoted error is manufacturer specified accuracy with respect to ITS-90 at time of laboratory calibration.                                                                                                                                                   No significant salinity drift detected. Salinity adjusted for effects of pressure adjustment. The quoted error is max[0.01, 1xOW uncertainty] in PSS-78.                                                                                                        201605311214282016053112142820160531121428  AO  ARCAADJP                                                                    20140721230517    IP                G�O�G�O�G�O�                AO  ARGQQCPL                                                                    20140721230517  QCP$                G�O�G�O�G�O�DFB7E           AO  ARGQQCPL                                                                    20140721230517  QCF$                G�O�G�O�G�O�0               UW  ARSQUWQC    WOD & nearby Argo as visual check                               20160531121428  IP                  G�O�G�O�G�O�                