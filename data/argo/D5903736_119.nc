CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       N2015-07-24T09:15:51Z AOML 3.0 creation; 2016-05-31T19:14:44Z UW 3.1 conversion     
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
_FillValue                    �4Argo profile    3.1 1.2 19500101000000  20150724091551  20160531121444  5903736 US ARGO PROJECT                                                 STEPHEN RISER                                                   PRES            TEMP            PSAL               wA   AO  4051_7090_119                   2C  D   APEX                            5368                            041511                          846 @�b=r�1   @�b> ��@3^��"���dZ^5?|�1   GPS     Primary sampling: mixed [deeper than nominal 985dbar: discrete; nominal 985dbar to surface: 2dbar-bin averaged]                                                                                                                                                    wA   A   A   @�33@�  A   A   A@  A`  A�  A�  A�  A�  A�  A�  A�  A�  B   BffB  B  B��B(  B0  B8��B?��BH  BP  BX  B`  Bh  Bp  Bx  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B���B�  B�  B�  B�  B�  B�  C   C  C  C  C  C
  C  C  C  C  C  C  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD�CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  Dy�D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D��D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dt� Dt�fDy��D��D�@ D�vfD�ٚD�� D�<�D�` D�� D��3D�@ D���D�� D��fD�0 D�c3D��3D� D�VfD�3D�� 111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  @��@��R@��RA\)A?\)A_\)A\)A��A��A��A��AϮA߮A�A��B=pB�
B�
Bp�B'�
B/�
B8��B?p�BG�
BO�
BW�
B_�
Bg�
Bo�
Bw�
B�
B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B�RB��B��B��B��B��B��B��C��C��C��C��C	��C��C��C��C��C��C��C��C��C��C��C��C!��C#��C%��C'��C)��C+��C-��C/��C1��C3��C5��C7��C9��C;��C=��C?��CA��CD]CE��CG��CI��CK��CM��CO��CQ��CS��CU��CW��CY��C[��C]��C_��Ca��Cc��Ce��Cg��Ci��Ck��Cm��Co��Cq��Cs��Cu��Cw��Cy��C{��C}��C��C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C��C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���D }qD �qD}qD�qD}qD�qD}qD�qD}qD�qDwD�qD}qD�qD}qD�qD}qD�qD	}qD	�qD
}qD
�qD}qD�qD}qD�qD}qD�qD}qD�D}qD�qD}qD�qD}qD�qD}qD�qD}qD�qD}qD�qD}qD�qD}qD�qD}qD�qD}qD�qD}qD�qD}qD�qD}qD�qD}qD�qD}qD�qD}qD�qD}qD�qD }qD �qD!}qD!�qD"}qD"�qD#}qD#�qD$}qD$�qD%}qD%�qD&}qD&�qD'}qD'�qD(}qD(�qD)}qD)�qD*}qD*�qD+}qD+�qD,}qD,�qD-}qD-�qD.}qD.�qD/}qD/�qD0}qD0�qD1}qD1�qD2}qD2�qD3}qD3�qD4}qD4�qD5}qD5�qD6}qD6�qD7}qD7�qD8}qD8�qD9}qD9�qD:}qD:�qD;}qD;�qD<}qD<�qD=}qD=�qD>}qD>�qD?}qD?�qD@}qD@�qDA}qDA�qDB}qDB�qDC}qDC�qDD}qDD�qDE}qDE�qDF}qDF�qDG}qDG�qDH}qDH�qDI}qDI�qDJ}qDJ�qDK}qDK�qDL}qDL�qDM}qDM�qDN}qDN�qDO}qDO�qDP}qDP�qDQ}qDQ�qDR}qDR�qDS}qDS�qDT}qDT�qDU}qDU�qDV}qDV�qDW}qDW�qDX}qDX�qDY}qDY�qDZ}qDZ�qD[}qD[�qD\}qD\�qD]}qD]�qD^}qD^�qD_}qD_�qD`}qD`�qDa}qDa�qDb}qDb�qDc}qDc�qDd}qDd�qDe}qDe�qDf}qDf�qDg}qDg�qDh}qDh�qDi}qDi�qDj}qDj�qDk}qDk�qDl}qDl�qDm}qDm�qDn}qDn�qDo}qDo�qDp}qDp�qDq}qDq�qDr}qDr�qDs}qDs�qDt}qDt��Dy�>D�RD�>�D�uD��RD��D�;�D�^�D���D���D�>�D���D�޸D��D�.�D�a�D���D��D�UD��D���111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  @��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��A���A�ƨA߼jAߕ�A�p�A�XA�7LA�/A�&�A��A�oA�
=A���A�|�AݮAݏ\A�~�A�^5A�%A�VA��TA���A��#A֏\AլA�|�A��mA�E�A��#A҇+A�  A�?}A�M�A��A��A��A��mA�r�A���A�A�"�A�jAʓuA�XAɓuAȴ9A���A��mA��
A�XA��mA��/A�p�A���A�;dA�33A���A�  A��!A�Q�A�  A��uA���A�z�A��A��#A�A���A�;dA�oA�K�A�5?A�1'A��A�l�A��A�S�A�A��!A���A��FA���A�-A��A�bA�r�A��;A��HA�A�bNA�l�A��`A�C�A��A�l�A��A��mA��^A��A�bNA�
=A�ZA�~�A�"�A��A��A�ffA�1A�?}A���A�r�A�dZA���A�A�VA���A��A��+A��HA��jA�
=A~1Azr�AvVAsO�Aq�PAo�mAl�Aj�DAh��Af�Aa�-A_A^�A\v�AZ�RAX1AU��AT$�AS`BAR5?AOXALffAI"�AF~�ACS�AB�+AA�A?
=A<�A<5?A;�;A9��A8�DA7�A6 �A4^5A2�jA0{A-|�A,I�A+\)A+A*1A(A'�PA&�!A&=qA$��A#XA!�A �AK�A��At�A��A�/A�A�hA��Ap�A��A�uAXA��Ax�A`BA�A��AJA�wA�-A�PA�PA\)A��A%A�AhsA�A��A��A^5A��A�A
��A
  A	p�A�mA��A��AO�A�^AXA�+AA�A��A9XA\)A $�@�ȴ@��!@��@�7L@��^@���@���@��@�-@���@�C�@�
=@�z�@�/@�M�@�@���@ް!@��@�?}@۝�@���@ٙ�@��/@�z�@׾w@ָR@ԃ@ӕ�@ӥ�@�;d@�(�@�"�@�=q@�`B@�"�@�ff@��^@���@�\)@��@�j@��9@���@��;@�1@� �@��H@�J@�Ĝ@��R@�`B@�Z@�r�@�r�@��D@�O�@��@�`B@��@���@��D@�(�@��@��\@�^5@�1'@���@��@���@��
@�K�@�-@��@��@�{@�J@��@���@��!@���@��h@�?}@�&�@���@�b@�
=@���@�n�@�J@��#@�G�@��
@��@���@�V@���@�@��@��u@��@���@�?}@���@��u@��;@��@���@���@���@�$�@���@�{@���@��^@�V@�r�@���@�l�@�|�@�33@��P@���@��
@��@���@�  @�b@� �@�9X@�A�@�r�@��@��D@���@���@��j@���@�z�@�A�@�b@��@�S�@��@�@�ȴ@���@���@���@�n�@��@�p�@�&�@��`@��j@��@�1'@���@��F@���@�l�@�C�@���@��R@���@���@�~�@�v�@�n�@�n�@�V@�ff@�ff@���@��h@�7L@���@���@�r�@�I�@��m@��@�|�@�C�@�+@�
=@�@��y@��R@���@�v�@�$�@��T@�hs@���@�Ĝ@���@�j@� �@�  @���@��;@���@�\)@�+@��@�
=@��y@���@��y@��H@�ȴ@��+@��+@�~�@�=q@���@�O�@��@��`@��u@�(�@���@��F@���@�\)@�;d@�
=@���@���@�n�@�J@���@�@���@�`B@���@���@���@��u@�j@���@�Q�@� �@��m@���@��@�;d@�33@�~�@�J@���@���@��@��#@���@��^@���@��7@�X@�V@���@�z�@�Q�@�1'@��;@�|�@�K�@�o@�$�@��@xb@nȴ@d�@[��@U@PA�@Jn�@C�m@9��@3o@*~�@$�j@ Q�@z�@ff@"�@ȴ@33@ �111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  A���A�ƨA߼jAߕ�A�p�A�XA�7LA�/A�&�A��A�oA�
=A���A�|�AݮAݏ\A�~�A�^5A�%A�VA��TA���A��#A֏\AլA�|�A��mA�E�A��#A҇+A�  A�?}A�M�A��A��A��A��mA�r�A���A�A�"�A�jAʓuA�XAɓuAȴ9A���A��mA��
A�XA��mA��/A�p�A���A�;dA�33A���A�  A��!A�Q�A�  A��uA���A�z�A��A��#A�A���A�;dA�oA�K�A�5?A�1'A��A�l�A��A�S�A�A��!A���A��FA���A�-A��A�bA�r�A��;A��HA�A�bNA�l�A��`A�C�A��A�l�A��A��mA��^A��A�bNA�
=A�ZA�~�A�"�A��A��A�ffA�1A�?}A���A�r�A�dZA���A�A�VA���A��A��+A��HA��jA�
=A~1Azr�AvVAsO�Aq�PAo�mAl�Aj�DAh��Af�Aa�-A_A^�A\v�AZ�RAX1AU��AT$�AS`BAR5?AOXALffAI"�AF~�ACS�AB�+AA�A?
=A<�A<5?A;�;A9��A8�DA7�A6 �A4^5A2�jA0{A-|�A,I�A+\)A+A*1A(A'�PA&�!A&=qA$��A#XA!�A �AK�A��At�A��A�/A�A�hA��Ap�A��A�uAXA��Ax�A`BA�A��AJA�wA�-A�PA�PA\)A��A%A�AhsA�A��A��A^5A��A�A
��A
  A	p�A�mA��A��AO�A�^AXA�+AA�A��A9XA\)A $�@�ȴ@��!@��@�7L@��^@���@���@��@�-@���@�C�@�
=@�z�@�/@�M�@�@���@ް!@��@�?}@۝�@���@ٙ�@��/@�z�@׾w@ָR@ԃ@ӕ�@ӥ�@�;d@�(�@�"�@�=q@�`B@�"�@�ff@��^@���@�\)@��@�j@��9@���@��;@�1@� �@��H@�J@�Ĝ@��R@�`B@�Z@�r�@�r�@��D@�O�@��@�`B@��@���@��D@�(�@��@��\@�^5@�1'@���@��@���@��
@�K�@�-@��@��@�{@�J@��@���@��!@���@��h@�?}@�&�@���@�b@�
=@���@�n�@�J@��#@�G�@��
@��@���@�V@���@�@��@��u@��@���@�?}@���@��u@��;@��@���@���@���@�$�@���@�{@���@��^@�V@�r�@���@�l�@�|�@�33@��P@���@��
@��@���@�  @�b@� �@�9X@�A�@�r�@��@��D@���@���@��j@���@�z�@�A�@�b@��@�S�@��@�@�ȴ@���@���@���@�n�@��@�p�@�&�@��`@��j@��@�1'@���@��F@���@�l�@�C�@���@��R@���@���@�~�@�v�@�n�@�n�@�V@�ff@�ff@���@��h@�7L@���@���@�r�@�I�@��m@��@�|�@�C�@�+@�
=@�@��y@��R@���@�v�@�$�@��T@�hs@���@�Ĝ@���@�j@� �@�  @���@��;@���@�\)@�+@��@�
=@��y@���@��y@��H@�ȴ@��+@��+@�~�@�=q@���@�O�@��@��`@��u@�(�@���@��F@���@�\)@�;d@�
=@���@���@�n�@�J@���@�@���@�`B@���@���@���@��u@�j@���@�Q�@� �@��m@���@��@�;d@�33@�~�@�J@���@���@��@��#@���@��^@���@��7@�X@�V@���@�z�@�Q�@�1'@��;@�|�@�K�@�o@�$�@��@xb@nȴ@d�@[��@U@PA�@Jn�@C�m@9��@3o@*~�@$�j@ Q�@z�@ff@"�@ȴ@33@ �111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  ;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oB
<jB
<jB
<jB
=qB
=qB
>wB
?}B
@�B
@�B
A�B
B�B
D�B
I�B
[#B
r�B
q�B
q�B
r�B
t�B
��B
��B �Bz�B�
B��B�FB��BoB&�Bk�B��B�9B��B��BɺB�XB��B�oB�+BffBS�Bp�B��B�XB�}B�RB��B��B�7Bo�Bp�B|�B��B�HBB+B\B�B�B�B�B�B"�B)�B"�B�B�B�B{B
=BBB  B��B�BÖB��B��B��B��B��B�B�3B�9B�9B�?B�!B��B��B��B�JB�B{�Bu�Bn�B_;BK�BC�BuB��B��BiyB?}B5?BhB
��B
�-B
��B
�JB
�7B
�B
u�B
ffB
ZB
M�B
@�B
;dB
7LB
,B
VB
  B	�B	�B	��B	�B	��B	��B	�1B	z�B	m�B	bNB	Q�B	A�B	:^B	0!B	$�B	�B	bB	%B	B��B�B�`B�5B�B��B��B��B��B��B��B��B��B��B��BɺBȴBȴBȴBɺB��B��B��B��B��B��B��B��B��B��B��B��B�dB�B��B��B��B��B�dB�^B�dB�^B�LB�LB�FB�^B�jB�^B�wB��B��B��B��B�
B�B��B��BɺBɺBɺB��B��B��B��B��B��BĜB��B�qB�dB�LB�XBÖBɺBƨBĜBĜB�qB�LB�B��B�B�-B�LB�?B��B��B�B��B��B�oB�\B�1B� B{�Bz�Bz�B}�B�B�+B�1B�7B�VB�bB�\B�\B�VB�\B�\B��B��B��B��B��B��B��B�{B�%B�B�=B��B��B�B�B�-B�LBĜBĜBÖB�}B�^B�XB�^B��BǮB��B�#B�;B�;B�yB�B�B�B��B��B�B�B�`B�mB�B�B��B��B		7B	PB	\B	hB	uB	�B	�B	�B	#�B	(�B	+B	-B	/B	0!B	8RB	>wB	A�B	A�B	A�B	@�B	E�B	F�B	F�B	H�B	G�B	E�B	G�B	I�B	K�B	VB	YB	ZB	XB	W
B	XB	YB	ZB	\)B	aHB	dZB	dZB	e`B	ffB	hsB	l�B	s�B	v�B	x�B	{�B	~�B	� B	�B	�B	�B	�B	�B	�B	�B	�%B	�7B	�=B	�DB	�VB	�bB	�bB	�oB	�{B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�!B	�-B	�9B	�9B	�?B	�?B	�FB	�LB	�XB	�jB	�wB	��B	��B	��B	��B	B	ĜB	ȴB	ɺB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�
B	�B	�)B	�/B	�;B	�;B	�BB	�HB	�NB	�TB	�ZB	�ZB	�`B	�`B	�`B	�`B	�fB	�mB	�yB	�yB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
B
B
B
B
B
B
%B
+B
+B
+B
+B
1B
1B
1B
1B
	7B

=B

=B

=B
DB
DB
JB
PB
VB
�B
�B
&�B
/B
8RB
=qB
A�B
E�B
J�B
R�B
XB
`BB
e`B
iyB
m�B
r�B
t�B
y�B
|�B
� 111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  B
<uB
<uB
<rB
={B
={B
>�B
?�B
@�B
@�B
A�B
B�B
D�B
I�B
[,B
r�B
q�B
q�B
r�B
t�B
��B
��B �Bz�B�B��B�KB��BqB&�Bk�B��B�?B��B��BɻB�\B��B�tB�,BfkBS�Bp�B��B�_B�B�[B��B��B�8Bo�Bp�B|�B��B�PB&B.BcB�B�B�B�B�B"�B*B"�B�B�B�B�B
ABBB B��B�BÙB��B�B��B��B��B�B�6B�AB�AB�CB�%B��B��B��B�OB� B{�Bu�Bn�B_@BK�BC�B{B��B��Bi|B?�B5DBlB
��B
�3B
��B
�RB
�?B
�B
u�B
foB
Z'B
M�B
@�B
;oB
7VB
,B
aB
 B	�B	�-B	��B	�#B	��B	��B	�BB	z�B	m�B	b_B	Q�B	A�B	:pB	04B	$�B	�B	uB	:B	B��B�B�vB�MB�'B�B�B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B�B�B�B�B�B�B��B��B�{B�3B��B��B��B��B��B�wB�}B�xB�dB�gB�]B�wB��B�wB��B��B��B�B�B�!B�&B�B��B��B��B��B��B��B�B��B��B��BĴB��B��B�}B�dB�oBíB��BƼBĵBĴB��B�bB�%B�B�B�DB�eB�YB�B��B�!B��B��B��B�vB�MB�B|Bz�Bz�B~B�+B�HB�MB�PB�oB�|B�uB�xB�pB�vB�vB��B� B�B��B��B��B��B��B�AB�5B�UB��B��B�!B�2B�DB�dBĴBĳBïB��B�uB�oB�wB��B��B��B�:B�SB�PB�B�B�B��B��B��B��B��B�tB�B�B�B��B��B		IB	bB	qB	zB	�B	�B	�B	�B	#�B	)B	+B	-"B	/0B	04B	8dB	>�B	A�B	A�B	A�B	@�B	E�B	F�B	F�B	H�B	G�B	E�B	G�B	I�B	K�B	VB	Y(B	Z0B	X B	WB	X B	Y(B	Z0B	\8B	aYB	dkB	dlB	eoB	fvB	h�B	l�B	s�B	v�B	x�B	{�B		B	�B	�B	�B	�B	�B	�B	�!B	�)B	�5B	�FB	�NB	�VB	�gB	�tB	�pB	�}B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�#B	�.B	�:B	�FB	�GB	�KB	�MB	�QB	�ZB	�dB	�yB	��B	��B	��B	��B	��B	B	ĬB	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�B	�B	�B	�B	�)B	�6B	�=B	�IB	�GB	�OB	�VB	�[B	�aB	�hB	�gB	�kB	�mB	�lB	�kB	�qB	�yB	�B	�B	�B	�B	�B	��B	�B	�B	�B	�B	�B	�B	�B	��B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�B
B
B
B
B
"B
)B
3B
:B
:B
7B
7B
=B
?B
=B
?B
	DB

KB

IB

JB
NB
OB
VB
ZB
aB
�B
�B
&�B
/'B
8]B
={B
A�B
E�B
J�B
R�B
XB
`MB
elB
i�B
m�B
r�B
t�B
y�B
|�B
�
111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  <#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
PRES            TEMP            PSAL            PRES_ADJUSTED = PRES - dP                                                                                                                                                                                                                                       none                                                                                                                                                                                                                                                            PSAL_ADJUSTED = sw_salt( sw_cndr(PSAL,TEMP,PRES), TEMP, PRES_ADJUSTED )                                                                                                                                                                                         dP =0.04 dbar.                                                                                                                                                                                                                                                  none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            Pressures adjusted by using pressure offset at the sea surface. The quoted error is manufacturer specified accuracy in dbar.                                                                                                                                    The quoted error is manufacturer specified accuracy with respect to ITS-90 at time of laboratory calibration.                                                                                                                                                   No significant salinity drift detected. Salinity adjusted for effects of pressure adjustment. The quoted error is max[0.01, 1xOW uncertainty] in PSS-78.                                                                                                        201605311214442016053112144420160531121444  AO  ARCAADJP                                                                    20150724091551    IP                G�O�G�O�G�O�                AO  ARGQQCPL                                                                    20150724091551  QCP$                G�O�G�O�G�O�5F03E           AO  ARGQQCPL                                                                    20150724091551  QCF$                G�O�G�O�G�O�0               UW  ARSQUWQC    WOD & nearby Argo as visual check                               20160531121444  IP                  G�O�G�O�G�O�                