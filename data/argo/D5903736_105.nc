CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       N2015-03-02T20:16:03Z AOML 3.0 creation; 2016-05-31T19:14:42Z UW 3.1 conversion     
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
_FillValue                    �Argo profile    3.1 1.2 19500101000000  20150302201603  20160531121442  5903736 US ARGO PROJECT                                                 STEPHEN RISER                                                   PRES            TEMP            PSAL               iA   AO  4051_7090_105                   2C  D   APEX                            5368                            041511                          846 @�>_8!`1   @�>_�)��@4,�C���dfM���1   GPS     Primary sampling: mixed [deeper than nominal 985dbar: discrete; nominal 985dbar to surface: 2dbar-bin averaged]                                                                                                                                                    iA   A   A   @���@���A   A   A@  A`  A�  A�  A�  A�  A�  A�  A�  A�  B   B  B  B  B   B(  B0  B8  B@  BH  BP  BX  B`  Bh  Bp  Bx  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  C   C  C  C  C  C
  C  C  C  C  C  C  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DOfDO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dt� Dt�fDy��D�3D�C3D��fD��fD��fD�33D��3D��3D��fD�c3D�� D�� D�3D�P Dڌ�D๚D�fD�C3D�` 11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   @�Q�@��@��RA\)A?\)A_\)A\)A��A��A��A��AϮA߮A�A��B�
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
B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��C��C��C��C��C	��C��C��C��C��C��C��C��C��C��C��C��C!��C#��C%��C'��C)��C+��C-��C/��C1��C3��C5��C7��C9��C;��C=��C?��CA��CC��CE��CG��CI��CK��CM��CO��CQ��CS��CU��CW��CY��C[��C]��C_��Ca��Cc��Ce��Cg��Ci��Ck��Cm��Co��Cq��Cs��Cu��Cw��Cy��C{��C}��C��C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C��C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C��C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���D }qD �qD}qD�qD}qD�qD}qD�qD}qD�qD}qD�qD}qD�qD}qD�qD}qD�qD	}qD	�qD
}qD
�qD}qD�qD}qD�qD}qD�qD}qD�qD}qD�qD}qD�qD}qD�qD}qD�qD}qD�qD}qD�qD}qD�qD}qD�qD}qD�qD}qD�qD}qD�qD}qD�qD}qD�qD}qD�qD}qD�qD}qD�qD}qD�qD }qD �qD!}qD!�qD"}qD"�qD#}qD#�qD$}qD$�qD%}qD%�qD&}qD&�qD'}qD'�qD(}qD(�qD)}qD)�qD*}qD*�qD+}qD+�qD,}qD,�qD-}qD-�qD.}qD.�qD/}qD/�qD0}qD0�qD1}qD1�qD2}qD2�qD3}qD3�qD4}qD4�qD5}qD5�qD6}qD6�qD7}qD7�qD8}qD8�qD9}qD9�qD:}qD:�qD;}qD;�qD<}qD<�qD=}qD=�qD>}qD>�qD?}qD?�qD@}qD@�qDA}qDA�qDB}qDB�qDC}qDC�qDD}qDD�qDE}qDE�qDF}qDF�qDG}qDG�qDH}qDH�qDI}qDI�qDJ}qDJ�qDK}qDK�qDL}qDL�qDM}qDM�qDN}qDO�DO}qDO�qDP}qDP�qDQ}qDQ�qDR}qDR�qDS}qDS�qDT}qDT�qDU}qDU�qDV}qDV�qDW}qDW�qDX}qDX�qDY}qDY�qDZ}qDZ�qD[}qD[�qD\}qD\�qD]}qD]�qD^}qD^�qD_}qD_�qD`}qD`�qDa}qDa�qDb}qDb�qDc}qDc�qDd}qDd�qDe}qDe�qDf}qDf�qDg}qDg�qDh}qDh�qDi}qDi�qDj}qDj�qDk}qDk�qDl}qDl�qDm}qDm�qDn}qDn�qDo}qDo�qDp}qDp�qDq}qDq�qDr}qDr�qDs}qDs�qDt}qDt��Dy�D��D�A�D��D��D��D�1�D���D���D��D�a�D���D�θD��D�N�Dڋ�D�RD�D�A�D�^�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   @��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��A�;dA�=qA�G�A�M�A�O�A�M�A�M�A�G�A�G�A�G�A�O�A�M�A��A�C�A��A�bA��A���A�jA��A���A���A��uA�v�A�M�A�7LA���A�$�A��A�l�A��`A��A���A���A��+A�VA��;A��A�|�A�v�A�9XA��`A���A���A��A�ffA�bNA�M�A�+A���A��FA�?}A�A���A��!A���A�t�A�C�A�ȴA�p�A�"�A��A��+A�n�A�hsA�jA�hsA�Q�A��wA�"�A��A��!A�hsA�?}A��A��9A�A��A��A�33A��A�A�A�-A��A�+A���A��RA�{A�1A��HA�G�A�M�A���A��A��jA���A���A�t�A���A��+A�33A��
A�K�A�"�A�v�A�+A�A��!A�
=A��
A�dZA��A�(�A�A�A�=qA�~�A���A��;A�;dA�|�A���A��HA�9XAzJAw\)At  Ar�uAo�Ao�Al�DAj-Ah��Ae;dAdM�Ad1Aa33A]�AZAW��AVffAR~�AQdZAN�yAM�AMO�AM�AMoAL~�AJVAG|�AEl�AB�yAA��A@ȴA?�A=�TA;�A:n�A8��A7XA4�A3��A1�mA/�FA/x�A/�A.�A+��A*��A)�A%�^A#dZA"��A"1'A!�TA!��A!?}A �A Q�A��Az�A&�A�mA33At�A�7AA&�A��AA�A/A�A�7AbNA&�A��Ar�AA�A�A
(�A	�mA	��A	�7A	+AM�A�;A�A/A�9A��AA�A�A&�AAx�A �j@��R@�`B@�I�@��y@���@�ƨ@���@�1'@�t�@�@���@�{@�hs@�V@�9@�ȴ@���@�bN@�\)@���@�n�@��@�G�@�@�l�@�7@�1@ߍP@޸R@ݺ^@���@�ƨ@���@�p�@�?}@�Z@�E�@ԛ�@� �@�l�@��@�@��@�ȴ@�+@���@��/@���@�n�@Ų-@�$�@�~�@��@�?}@�r�@���@�v�@�hs@���@�Q�@� �@�b@���@���@�o@�V@���@��9@�z�@�Q�@�|�@���@��@��H@���@��T@�x�@�G�@�%@�Ĝ@� �@�n�@�p�@�/@���@���@�z�@�bN@��@�ff@�{@��#@��-@��h@�O�@���@�r�@�(�@��@��P@�dZ@�;d@�5?@��T@��T@��#@���@��@��@���@�Ĝ@�r�@� �@��@�C�@�
=@��+@�{@�hs@��@�Ĝ@��@�bN@�9X@�  @��
@��F@��P@�t�@�S�@�K�@�;d@��y@���@�~�@��@���@�hs@�V@�Ĝ@��u@��@�Z@�I�@�I�@�  @���@��P@��P@�+@�^5@�=q@��+@��@�+@�E�@��h@��@���@��u@�(�@���@�|�@�dZ@��@�@���@��@���@�~�@�n�@��@��h@��@��@��@�1@�t�@�dZ@�K�@�C�@��@��@���@�~�@�V@�V@�=q@��@���@�G�@�/@��D@��@���@�dZ@�S�@�33@�33@�o@�@���@��@�ff@��T@�@���@�p�@��@���@�1'@���@��
@��F@��P@�o@���@�$�@���@�X@�&�@�V@��/@�Ĝ@��u@�Z@�(�@���@�t�@�S�@��@��H@��!@�=q@���@��#@���@��-@���@��@�`B@�O�@��@��9@�1'@��;@��w@�\)@�33@�
=@�ȴ@���@�~�@�V@�5?@�$�@�$�@�$�@�$�@�J@���@���@��7@�G�@��@�z�@�9X@� �@�b@��@��
@���@�;d@�7L@��u@xr�@o�;@ihs@a�#@Yx�@PĜ@G�P@=/@7�@0�u@*�\@%�@K�@�@V@n�@�w@�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   A�;dA�=qA�G�A�M�A�O�A�M�A�M�A�G�A�G�A�G�A�O�A�M�A��A�C�A��A�bA��A���A�jA��A���A���A��uA�v�A�M�A�7LA���A�$�A��A�l�A��`A��A���A���A��+A�VA��;A��A�|�A�v�A�9XA��`A���A���A��A�ffA�bNA�M�A�+A���A��FA�?}A�A���A��!A���A�t�A�C�A�ȴA�p�A�"�A��A��+A�n�A�hsA�jA�hsA�Q�A��wA�"�A��A��!A�hsA�?}A��A��9A�A��A��A�33A��A�A�A�-A��A�+A���A��RA�{A�1A��HA�G�A�M�A���A��A��jA���A���A�t�A���A��+A�33A��
A�K�A�"�A�v�A�+A�A��!A�
=A��
A�dZA��A�(�A�A�A�=qA�~�A���A��;A�;dA�|�A���A��HA�9XAzJAw\)At  Ar�uAo�Ao�Al�DAj-Ah��Ae;dAdM�Ad1Aa33A]�AZAW��AVffAR~�AQdZAN�yAM�AMO�AM�AMoAL~�AJVAG|�AEl�AB�yAA��A@ȴA?�A=�TA;�A:n�A8��A7XA4�A3��A1�mA/�FA/x�A/�A.�A+��A*��A)�A%�^A#dZA"��A"1'A!�TA!��A!?}A �A Q�A��Az�A&�A�mA33At�A�7AA&�A��AA�A/A�A�7AbNA&�A��Ar�AA�A�A
(�A	�mA	��A	�7A	+AM�A�;A�A/A�9A��AA�A�A&�AAx�A �j@��R@�`B@�I�@��y@���@�ƨ@���@�1'@�t�@�@���@�{@�hs@�V@�9@�ȴ@���@�bN@�\)@���@�n�@��@�G�@�@�l�@�7@�1@ߍP@޸R@ݺ^@���@�ƨ@���@�p�@�?}@�Z@�E�@ԛ�@� �@�l�@��@�@��@�ȴ@�+@���@��/@���@�n�@Ų-@�$�@�~�@��@�?}@�r�@���@�v�@�hs@���@�Q�@� �@�b@���@���@�o@�V@���@��9@�z�@�Q�@�|�@���@��@��H@���@��T@�x�@�G�@�%@�Ĝ@� �@�n�@�p�@�/@���@���@�z�@�bN@��@�ff@�{@��#@��-@��h@�O�@���@�r�@�(�@��@��P@�dZ@�;d@�5?@��T@��T@��#@���@��@��@���@�Ĝ@�r�@� �@��@�C�@�
=@��+@�{@�hs@��@�Ĝ@��@�bN@�9X@�  @��
@��F@��P@�t�@�S�@�K�@�;d@��y@���@�~�@��@���@�hs@�V@�Ĝ@��u@��@�Z@�I�@�I�@�  @���@��P@��P@�+@�^5@�=q@��+@��@�+@�E�@��h@��@���@��u@�(�@���@�|�@�dZ@��@�@���@��@���@�~�@�n�@��@��h@��@��@��@�1@�t�@�dZ@�K�@�C�@��@��@���@�~�@�V@�V@�=q@��@���@�G�@�/@��D@��@���@�dZ@�S�@�33@�33@�o@�@���@��@�ff@��T@�@���@�p�@��@���@�1'@���@��
@��F@��P@�o@���@�$�@���@�X@�&�@�V@��/@�Ĝ@��u@�Z@�(�@���@�t�@�S�@��@��H@��!@�=q@���@��#@���@��-@���@��@�`B@�O�@��@��9@�1'@��;@��w@�\)@�33@�
=@�ȴ@���@�~�@�V@�5?@�$�@�$�@�$�@�$�@�J@���@���@��7@�G�@��@�z�@�9X@� �@�b@��@��
@���@�;d@�7L@��u@xr�@o�;@ihs@a�#@Yx�@PĜ@G�P@=/@7�@0�u@*�\@%�@K�@�@V@n�@�w@�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   ;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oB�B�B�B�B�B�B�B�B�B�B�B�B�qB��BDB{B�B)�B?}BM�BJ�BQ�BVB\)B_;B_;BbNBffBgmBl�Br�Bw�Bx�By�Bz�B{�B}�B�B�B� B~�B~�B� B� B�B�B�B�B�1B�7B�DB�=B�=B�DB�JB�JB�JB�JB�PB��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B�JB�%B�Bv�BjBK�B;dB'�BoB�fB��BB�^B�B��B��B�hBu�BgmBaHBS�B;dBbB�#B��B�qB�DB�+B�B�Bz�Bw�Bm�BaHBP�B{B
�B
�BB
��B
ÖB
�RB
�B
�bB
n�B
R�B
%�B
PB	�B	�fB	��B	ɺB	�LB	��B	��B	�B	|�B	w�B	cTB	F�B	5?B	'�B	 �B	oB	JB	
=B	1B	+B	%B	B	B��B�B�B�`B�NB�;B�)B�B��B��B��BĜB�}B�RB�FB�!B�B�B��B��B��B�oB� Br�Bn�Bk�BjBiyBhsBgmBffBcTB[#BgmBbNB`BB\)B]/B[#B^5B`BBdZBffBffBe`BcTBaHB`BBcTBe`BdZBe`Be`BdZBdZBdZBdZBbNBaHB`BBcTBr�Bs�Bs�Bs�Bt�Bs�Bt�Bx�Bz�B|�B}�B}�B}�B� B�B�B�B�B�B�B�B�B�B�B�B�B�+B�PB�VB�\B�hB�{B��B��B��B��B�B�B�B�-B�?B�9B�9B�-B�'B�!B�B�B��B�B�B�B��B�B�B�?B�dBȴB��B�B�)B�HB�NB�NB�ZB�fB�B�B�B�B�B�B��B��B��B��B��B��B��B	B	B	B	hB	{B	�B	�B	�B	�B	 �B	 �B	!�B	$�B	%�B	%�B	%�B	(�B	-B	.B	/B	/B	/B	0!B	1'B	33B	5?B	6FB	9XB	:^B	:^B	B�B	E�B	E�B	E�B	H�B	I�B	M�B	N�B	P�B	Q�B	S�B	W
B	YB	ZB	^5B	`BB	dZB	hsB	jB	m�B	o�B	r�B	u�B	v�B	w�B	y�B	z�B	{�B	{�B	|�B	~�B	� B	�B	�B	�B	�B	�1B	�JB	�VB	�uB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�FB	�FB	�LB	�XB	�^B	�jB	�wB	ÖB	ĜB	ŢB	ŢB	ǮB	ɺB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�#B	�#B	�;B	�BB	�HB	�HB	�HB	�HB	�HB	�NB	�NB	�NB	�TB	�ZB	�fB	�fB	�fB	�mB	�sB	�yB	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
  B
  B
B
B
B
B
B
B
B
B
B
B
%B
+B
+B
+B
1B
1B
1B
1B
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

=B

=B
DB
DB
JB
JB
JB
JB
JB
JB
JB
PB
hB
�B
#�B
'�B
/B
2-B
9XB
@�B
F�B
N�B
S�B
XB
^5B
bNB
hsB
n�B
q�B
t�B
v�B
y�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   B�	B�B�B�B�	B�B�B�B�B�B�B�B�zB��BLB�B�B*B?�BM�BJ�BQ�BVB\.B_CB_CBbVBfpBgyBl�Br�Bw�Bx�By�Bz�B{�B}�B�B�B�B BB�B�B�B�B�B�'B�7B�AB�LB�CB�GB�OB�TB�UB�QB�SB�YB��B��B��B��B��B�B�B�B�B�B��B��B��B��B��B��B��B��B��B�SB�.B�Bv�Bj�BK�B;lB'�BuB�mB��BB�gB�B��B��B�lBu�BgpBaKBS�B;jBdB�&B��B�xB�GB�1B�&B�Bz�Bw�Bm�BaLBP�B�B
�B
�JB
��B
ÚB
�YB
�B
�lB
n�B
R�B
%�B
ZB	�B	�sB	��B	��B	�\B	��B	��B	�!B	|�B	w�B	ceB	F�B	5SB	(B	 �B	�B	]B	
QB	GB	@B	9B	1B	B��B��B�B�vB�fB�QB�BB�(B�B��B��BĵB��B�kB�_B�9B�4B�B�B��B��B��B�Br�Bn�Bk�Bj�Bi�Bh�Bg�Bf�BcoB[@Bg�BbkB`_B\HB]IB[@B^PB`_BdwBf�Bf�Be|BcqBaeB`^BcpBe|BdtBeBe{BdvBduBdwBdsBbhBafB`_BcnBr�Bs�Bs�Bs�Bt�Bs�Bt�Bx�Bz�B}	B~B~B~B�B�$B�&B�&B�&B�%B�'B�'B�'B�,B�8B�1B�2B�EB�iB�pB�yB��B��B��B��B��B�B�B�B�,B�CB�WB�PB�SB�BB�?B�9B�%B�"B�B�"B�&B�B�B�!B�,B�WB�|B��B�B�B�@B�\B�dB�fB�nB�{B�B�B�B��B�B��B��B��B��B�B�B�B�B	"B	'B	1B	}B	�B	�B	�B	�B	�B	 �B	 �B	!�B	$�B	%�B	%�B	%�B	)	B	- B	.'B	/*B	/.B	/.B	04B	19B	3DB	5RB	6VB	9hB	:rB	:rB	B�B	E�B	E�B	E�B	H�B	I�B	M�B	N�B	P�B	Q�B	TB	WB	Y*B	Z.B	^IB	`RB	diB	h�B	j�B	m�B	o�B	r�B	u�B	v�B	w�B	y�B	z�B	{�B	{�B	} B	
B	�B	�B	�B	�!B	�0B	�AB	�ZB	�cB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�(B	�UB	�SB	�YB	�fB	�lB	�wB	��B	áB	ĪB	ŰB	ůB	ǺB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�
B	�B	�B	�B	�B	�$B	�$B	�$B	�*B	�+B	�+B	�1B	�/B	�GB	�OB	�TB	�UB	�UB	�UB	�RB	�ZB	�\B	�ZB	�cB	�gB	�qB	�rB	�rB	�yB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B
 B
 B
B
B
 B
 B
$B
'B
%B
)B
+B
,B
/B
9B
9B
:B
@B
=B
>B
?B
@B
=B
=B
	BB
	BB
	CB

FB

FB

IB

FB

GB

IB
OB
QB
WB
XB
XB
TB
WB
VB
UB
ZB
uB
�B
#�B
'�B
/&B
29B
9bB
@�B
F�B
N�B
TB
XB
^<B
bYB
h}B
n�B
q�B
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
PRES            TEMP            PSAL            PRES_ADJUSTED = PRES - dP                                                                                                                                                                                                                                       none                                                                                                                                                                                                                                                            PSAL_ADJUSTED = sw_salt( sw_cndr(PSAL,TEMP,PRES), TEMP, PRES_ADJUSTED )                                                                                                                                                                                         dP =0.04 dbar.                                                                                                                                                                                                                                                  none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            Pressures adjusted by using pressure offset at the sea surface. The quoted error is manufacturer specified accuracy in dbar.                                                                                                                                    The quoted error is manufacturer specified accuracy with respect to ITS-90 at time of laboratory calibration.                                                                                                                                                   No significant salinity drift detected. Salinity adjusted for effects of pressure adjustment. The quoted error is max[0.01, 1xOW uncertainty] in PSS-78.                                                                                                        201605311214422016053112144220160531121442  AO  ARCAADJP                                                                    20150302201603    IP                G�O�G�O�G�O�                AO  ARGQQCPL                                                                    20150302201603  QCP$                G�O�G�O�G�O�DFB5E           AO  ARGQQCPL                                                                    20150302201603  QCF$                G�O�G�O�G�O�0               UW  ARSQUWQC    WOD & nearby Argo as visual check                               20160531121442  IP                  G�O�G�O�G�O�                