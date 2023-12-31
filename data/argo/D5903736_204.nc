CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             
   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       2017-12-12T01:00:56Z creation      
references        (http://www.argodatamgt.org/Documentation   comment       	free text      user_manual_version       3.1    Conventions       Argo-3.1 CF-1.6    featureType       trajectoryProfile      comment_dmqc_operator         (Matthew Alkire, University of Washington      @   	DATA_TYPE                  	long_name         	Data type      conventions       Argo reference table 1     
_FillValue                    6�   FORMAT_VERSION                 	long_name         File format version    
_FillValue                    6�   HANDBOOK_VERSION               	long_name         Data handbook version      
_FillValue                    6�   REFERENCE_DATE_TIME                 	long_name         !Date of reference for Julian days      conventions       YYYYMMDDHHMISS     
_FillValue                    6�   DATE_CREATION                   	long_name         Date of file creation      conventions       YYYYMMDDHHMISS     
_FillValue                    7   DATE_UPDATE                 	long_name         Date of update of this file    conventions       YYYYMMDDHHMISS     
_FillValue                    7   PLATFORM_NUMBER                   	long_name         Float unique identifier    conventions       WMO float identifier : A9IIIII     
_FillValue                    7,   PROJECT_NAME                  	long_name         Name of the project    
_FillValue                  @  74   PI_NAME                   	long_name         "Name of the principal investigator     
_FillValue                  @  7t   STATION_PARAMETERS           	            	long_name         ,List of available parameters for the station   conventions       Argo reference table 3     
_FillValue                  0  7�   CYCLE_NUMBER               	long_name         Float cycle number     conventions       =0...N, 0 : launch cycle (if exists), 1 : first complete cycle      
_FillValue         ��        7�   	DIRECTION                  	long_name         !Direction of the station profiles      conventions       -A: ascending profiles, D: descending profiles      
_FillValue                    7�   DATA_CENTRE                   	long_name         .Data centre in charge of float data processing     conventions       Argo reference table 4     
_FillValue                    7�   DC_REFERENCE                  	long_name         (Station unique identifier in data centre   conventions       Data centre convention     
_FillValue                     7�   DATA_STATE_INDICATOR                  	long_name         1Degree of processing the data have passed through      conventions       Argo reference table 6     
_FillValue                    8   	DATA_MODE                  	long_name         Delayed mode or real time data     conventions       >R : real time; D : delayed mode; A : real time with adjustment     
_FillValue                    8   PLATFORM_TYPE                     	long_name         Type of float      conventions       Argo reference table 23    
_FillValue                     8   FLOAT_SERIAL_NO                   	long_name         Serial number of the float     
_FillValue                     88   FIRMWARE_VERSION                  	long_name         Instrument firmware version    
_FillValue                     8X   WMO_INST_TYPE                     	long_name         Coded instrument type      conventions       Argo reference table 8     
_FillValue                    8x   JULD               	long_name         ?Julian day (UTC) of the station relative to REFERENCE_DATE_TIME    standard_name         time   units         "days since 1950-01-01 00:00:00 UTC     conventions       8Relative julian days with decimal part (as parts of day)   
resolution        >�EȠ      
_FillValue        A.�~       axis      T           8|   JULD_QC                	long_name         Quality on date and time   conventions       Argo reference table 2     
_FillValue                    8�   JULD_LOCATION                  	long_name         @Julian day (UTC) of the location relative to REFERENCE_DATE_TIME   units         "days since 1950-01-01 00:00:00 UTC     conventions       8Relative julian days with decimal part (as parts of day)   
resolution        >�EȠ      
_FillValue        A.�~            8�   LATITUDE               	long_name         &Latitude of the station, best estimate     standard_name         latitude   units         degree_north   
_FillValue        @�i�       	valid_min         �V�        	valid_max         @V�        axis      Y           8�   	LONGITUDE                  	long_name         'Longitude of the station, best estimate    standard_name         	longitude      units         degree_east    
_FillValue        @�i�       	valid_min         �f�        	valid_max         @f�        axis      X           8�   POSITION_QC                	long_name         ,Quality on position (latitude and longitude)   conventions       Argo reference table 2     
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
resolution        :�o     �  qt   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                    yh   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  {h   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                    �\   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  �\   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  0  �P   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                    ��   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                    ��   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                    ��   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  ,  ��   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                    ��   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                    ��   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                    ��   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                    ��   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  @  ��   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                    ��   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                    �   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    �   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar         �    HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar         �$   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�        �(   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    �,Argo profile    3.1 1.2 19500101000000  20171212010056  20190604094031  5903736 US ARGO PROJECT                                                 STEPHEN RISER                                                   PRES            TEMP            PSAL               �A   AO  4051                            2C  D   APEX                            5368                            041511                          846 @�</+��1   @�</�<�@4�?|�h�d�bM��1   GPS     Primary sampling: mixed [deeper than nominal 985dbar: discrete; nominal 985dbar to surface: 2dbar-bin averaged]                                                                                                                                                    �A   B   B   @�ff@�  A   A   A@  A`  A�  A�  A�  A�  A�  A�  A�  A�  B   B  B  B  B   B(  B0  B8  B@  BH  BP  BX  B`  Bh  Bp  Bx  B�  B�  B�  B�  B�  B�  B�  B�33B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  C   C  C  C  C  C
  C  C  C  C  C  C  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  DtffDys�D�
�D�@RD�"�D���D�RD�?
D��3D�ٚD���D�7
D��D��=D�fD�ED�W
D���D���D�=D�vD��q11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   @��@��R@��RA\)A?\)A_\)A\)A��A��A��A��AϮA߮A�A��B�
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
B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��C��C��C��C��C	��C��C��C��C��C��C��C��C��C��C��C��C!��C#��C%��C'��C)��C+��C-��C/��C1��C3��C5��C7��C9��C;��C=��C?��CA��CC��CE��CG��CI��CK��CM��CO��CQ��CS��CU��CW��CY��C[��C]��C_��Ca��Cc��Ce��Cg��Ci��Ck��Cm��Co��Cq��Cs��Cu��Cw��Cy��C{��C}��C��C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C��C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���D }qD �qD}qD�qD}qD�qD}qD�qD}qD�qD}qD�qD}qD�qD}qD�qD}qD�qD	}qD	�qD
}qD
�qD}qD�qD}qD�qD}qD�qD}qD�qD}qD�qD}qD�qD}qD�qD}qD�qD}qD�qD}qD�qD}qD�qD}qD�qD}qD�qD}qD�qD}qD�qD}qD�qD}qD�qD}qD�qD}qD�qD}qD�qD}qD�qD }qD �qD!}qD!�qD"}qD"�qD#}qD#�qD$}qD$�qD%}qD%�qD&}qD&�qD'}qD'�qD(}qD(�qD)}qD)�qD*}qD*�qD+}qD+�qD,}qD,�qD-}qD-�qD.}qD.�qD/}qD/�qD0}qD0�qD1}qD1�qD2}qD2�qD3}qD3�qD4}qD4�qD5}qD5�qD6}qD6�qD7}qD7�qD8}qD8�qD9}qD9�qD:}qD:�qD;}qD;�qD<}qD<�qD=}qD=�qD>}qD>�qD?}qD?�qD@}qD@�qDA}qDA�qDB}qDB�qDC}qDC�qDD}qDD�qDE}qDE�qDF}qDF�qDG}qDG�qDH}qDH�qDI}qDI�qDJ}qDJ�qDK}qDK�qDL}qDL�qDM}qDM�qDN}qDN�qDO}qDO�qDP}qDP�qDQ}qDQ�qDR}qDR�qDS}qDS�qDT}qDT�qDU}qDU�qDV}qDV�qDW}qDW�qDX}qDX�qDY}qDY�qDZ}qDZ�qD[}qD[�qD\}qD\�qD]}qD]�qD^}qD^�qD_}qD_�qD`}qD`�qDa}qDa�qDb}qDb�qDc}qDc�qDd}qDd�qDe}qDe�qDf}qDf�qDg}qDg�qDh}qDh�qDi}qDi�qDj}qDj�qDk}qDk�qDl}qDl�qDm}qDm�qDn}qDn�qDo}qDo�qDp}qDp�qDq}qDq�qDr}qDr�qDs}qDs�qDtc�DyqHD�	�D�?
D�!�D�əD�
D�=�D���D��RD��pD�5�D���D���D�D�C�D�U�D�ҏD��GD�;�D�t�D��)11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   @��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��A��
A��
A��A��A��A��/A��/A��/A��;A��;A��HA��HA��TA��TA��TA��TA��HA��HA��HA��;A��/A��/A��/A��/A��/A��#A��A���A�ƨA�ĜAɺ^AɶFAɣ�AɍPAɁA�jA�1'A��A�1AȸRA�K�AǋDA�G�A�7LA�Q�A�^5A�bNA��A�=qA�=qA�jA��A�ƨA��A��+A�(�A��A���A���A��7A�\)A���A���A���A���A���A��wA��A��A�=qA���A�hsA��#A�%A�^5A��!A��A��A��jA�x�A�ffA�Q�A�;dA�33A��A�+A�  A�bA��A�S�A�;dA��jA��wA��A�p�A�A�A�(�A�A��DA�A�A��A�t�A���A���A���A��`A��`A��jA��\AXA}p�Az��AxbAv�Ar��AqK�Ap�An�yAm�AlM�Aj�DAg��Ae�#Ad�+Ac�-Ab^5Ab9XA`^5A^�\A^^5A^=qA^{A]�A]��A\-AY�#AY;dAW%AU�PAS�TAP��AP�AN9XAJ�AI��AH�!AG�wAFJAB��A@1'A=\)A<5?A9�A8A�A7l�A6�A4  A2A1��A1%A/�A/+A-�hA,�!A,(�A+��A+�A);dA'oA%A%|�A%S�A$��A$bNA#�-A"VA   A�A��AE�AdZA�!A��A�PA�A�uA�FA?}A7LA7LA/A+A&�A�A+A;dA
=A��A��A��A��At�A�A��A$�A�A5?A;dA�PAbA
9XA  A"�AȴAffA�A�PA��AG�@��^@��@���@�5?@��@�@�E�@�z�@�\@��T@�@�@畁@�@�"�@�J@���@�O�@�I�@� �@��@���@�\)@ݺ^@��@��@�p�@�Z@��/@� �@Ӿw@ӕ�@�ƨ@�j@ԛ�@Դ9@�%@�7L@�@��@���@��H@�hs@��@ϕ�@���@�`B@���@ʸR@�;d@�33@���@�1'@�|�@��\@��@��u@� �@��;@�\)@��@�o@���@�Z@��u@��u@��@��@���@�@��@��#@��u@�1@���@��!@��@�33@���@���@��!@��+@�$�@���@�x�@��7@�&�@�  @�K�@�K�@�~�@��@�7L@� �@���@�@��H@�ȴ@�~�@���@��@��@��@�+@�n�@�{@���@��h@�x�@�p�@�X@��@�9X@��
@���@�t�@��@���@��F@�ƨ@���@�C�@��y@�ȴ@��R@��R@���@���@��+@��\@���@��\@��+@�v�@�n�@�ff@�^5@�V@�E�@�-@�J@���@���@��@�O�@��u@�b@�b@��m@�|�@���@�V@��T@��T@��#@�@�hs@��/@��9@��j@���@�z�@�9X@�A�@�Q�@�I�@��@�ƨ@��;@�1@��@� �@�b@��@��m@��@�|�@�K�@���@�V@�M�@�M�@�M�@�M�@�=q@���@�%@���@�Z@��w@���@�;d@�
=@�ȴ@���@�n�@�=q@��@�@���@�G�@��@���@�Q�@�z�@�j@�Z@�  @��F@�"�@�ȴ@��+@�J@��#@�p�@�X@��@�%@���@��@��9@�bN@�b@��@�dZ@�;d@�@���@���@�ff@�M�@�{@���@�p�@�X@��@��@�j@�A�@��;@�ƨ@��@��@��P@���@�\)@��@�ȴ@��\@�ff@�^5@�E�@�$�@��+@�5?@��@���@��@�G�@��@�V@�p�@�7L@��`@��u@�I�@�(�@� �@���@���@y�@q�S@i��@c�f@X�z@O��@IY�@A�@:�h@4�K@.��@)<6@$��@ ��@�4@/�@
=@�r@
��@��11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111   A��
A��
A��A��A��A��/A��/A��/A��;A��;A��HA��HA��TA��TA��TA��TA��HA��HA��HA��;A��/A��/A��/A��/A��/A��#A��A���A�ƨA�ĜAɺ^AɶFAɣ�AɍPAɁA�jA�1'A��A�1AȸRA�K�AǋDA�G�A�7LA�Q�A�^5A�bNA��A�=qA�=qA�jA��A�ƨA��A��+A�(�A��A���A���A��7A�\)A���A���A���A���A���A��wA��A��A�=qA���A�hsA��#A�%A�^5A��!A��A��A��jA�x�A�ffA�Q�A�;dA�33A��A�+A�  A�bA��A�S�A�;dA��jA��wA��A�p�A�A�A�(�A�A��DA�A�A��A�t�A���A���A���A��`A��`A��jA��\AXA}p�Az��AxbAv�Ar��AqK�Ap�An�yAm�AlM�Aj�DAg��Ae�#Ad�+Ac�-Ab^5Ab9XA`^5A^�\A^^5A^=qA^{A]�A]��A\-AY�#AY;dAW%AU�PAS�TAP��AP�AN9XAJ�AI��AH�!AG�wAFJAB��A@1'A=\)A<5?A9�A8A�A7l�A6�A4  A2A1��A1%A/�A/+A-�hA,�!A,(�A+��A+�A);dA'oA%A%|�A%S�A$��A$bNA#�-A"VA   A�A��AE�AdZA�!A��A�PA�A�uA�FA?}A7LA7LA/A+A&�A�A+A;dA
=A��A��A��A��At�A�A��A$�A�A5?A;dA�PAbA
9XA  A"�AȴAffA�A�PA��AG�@��^@��@���@�5?@��@�@�E�@�z�@�\@��T@�@�@畁@�@�"�@�J@���@�O�@�I�@� �@��@���@�\)@ݺ^@��@��@�p�@�Z@��/@� �@Ӿw@ӕ�@�ƨ@�j@ԛ�@Դ9@�%@�7L@�@��@���@��H@�hs@��@ϕ�@���@�`B@���@ʸR@�;d@�33@���@�1'@�|�@��\@��@��u@� �@��;@�\)@��@�o@���@�Z@��u@��u@��@��@���@�@��@��#@��u@�1@���@��!@��@�33@���@���@��!@��+@�$�@���@�x�@��7@�&�@�  @�K�@�K�@�~�@��@�7L@� �@���@�@��H@�ȴ@�~�@���@��@��@��@�+@�n�@�{@���@��h@�x�@�p�@�X@��@�9X@��
@���@�t�@��@���@��F@�ƨ@���@�C�@��y@�ȴ@��R@��R@���@���@��+@��\@���@��\@��+@�v�@�n�@�ff@�^5@�V@�E�@�-@�J@���@���@��@�O�@��u@�b@�b@��m@�|�@���@�V@��T@��T@��#@�@�hs@��/@��9@��j@���@�z�@�9X@�A�@�Q�@�I�@��@�ƨ@��;@�1@��@� �@�b@��@��m@��@�|�@�K�@���@�V@�M�@�M�@�M�@�M�@�=q@���@�%@���@�Z@��w@���@�;d@�
=@�ȴ@���@�n�@�=q@��@�@���@�G�@��@���@�Q�@�z�@�j@�Z@�  @��F@�"�@�ȴ@��+@�J@��#@�p�@�X@��@�%@���@��@��9@�bN@�b@��@�dZ@�;d@�@���@���@�ff@�M�@�{@���@�p�@�X@��@��@�j@�A�@��;@�ƨ@��@��@��P@���@�\)@��@�ȴ@��\@�ff@�^5@�E�@�$�@��+@�5?@��@���@��@�G�@��@�V@�p�@�7L@��`@��u@�I�@�(�@� �G�O�@���@y�@q�S@i��@c�f@X�z@O��@IY�@A�@:�h@4�K@.��@)<6@$��@ ��@�4@/�@
=@�r@
��@��11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111   ;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oG�O�;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oB)�B)�B)�B+B)�B)�B+B)�B)�B+B+B+B+B+B,B,B-B-B.B0!B1'B1'B1'B2-B2-B2-B2-B1'B2-B2-B1'B0!B.B-B-B,B-B.B.B.B/B0!B0!B0!BC�Be`BhsBk�B]/BW
BQ�BN�BO�BQ�BR�BR�BO�BS�BS�BR�BR�BN�BD�BA�B>wB;dB9XB8RB8RB6FB49B33B1'B,B!�B�B	7B��B�B�B�B�yB�sB�mB�ZB��B�qB�B�B��B�wB��B��BgmB/B�B+B
��B
�B
�B
ĜB
�B
��B
�VB
�B
u�B
gmB
VB
B�B
7LB
@�B
8RB
,B
0!B
$�B
"�B
�B
{B
+B	��B	�B	�/B	��B	ǮB	��B	�RB	�?B	�B	��B	��B	��B	��B	��B	��B	�{B	�7B	�B	w�B	m�B	bNB	T�B	N�B	G�B	<jB	8RB	5?B	1'B	&�B	�B	1B��B��B�yB�ZB�/B��BɺB�wB��BŢB��B�^B�-B�B��B��B��B��B��B��B��B��B��B�{B�hB�\B�DB�7B�1B�%B�B�B� B}�B|�Bz�Bz�B}�B�B�B�B�+B�=B�VB�hB�{B��B��B��B�oB�oB�{B�{B��B��B��B��B��B��B��B�JB}�By�By�B|�B|�Bz�Bt�Bn�Bo�Bl�Be`B\)BXBR�BP�BM�BL�BM�BS�BW
BZB[#B[#BYBVBYBZBYBYBYBW
BXBYBW
BXBYBXBYB\)B_;BbNBhsBiyBjBl�Bn�Bu�By�B{�B{�Bz�Bx�Bw�Bw�Bv�Bu�Bt�Bt�B{�B�B�B�B�B�7B�1B�=B�JB�bB��B��B��B��B�B�!B�FB�RB�LB�FB�?B�FB�dB�wB�}BÖB��B��B��B��B��B��B�B�)B�TB�fB�mB�B�B��B��B��B��B��B	  B	B	+B	%B	%B	B	B	+B	JB	oB	{B	{B	�B	�B	�B	�B	�B	!�B	'�B	+B	-B	1'B	2-B	49B	8RB	B�B	H�B	I�B	J�B	K�B	L�B	L�B	M�B	M�B	P�B	YB	bNB	e`B	ffB	iyB	jB	l�B	n�B	o�B	o�B	p�B	q�B	r�B	r�B	r�B	r�B	u�B	y�B	~�B	� B	� B	�B	�B	�+B	�+B	�+B	�1B	�DB	�hB	�uB	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�!B	�'B	�-B	�-B	�FB	�RB	�XB	�^B	�dB	�qB	��B	B	B	B	B	B	��B	��B	�}B	��B	�}B	�}B	B	ÖB	ŢB	ǮB	ǮB	ɺB	��B	��B	��B	��B	��B	��B	�B	�B	�B	�#B	�)B	�)B	�5B	�5B	�;B	�HB	�NB	�`B	�fB	�sB	�yB	�yB	�yB	�yB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
  B
  B
B
B
B
B
+B
+B
+B
	7B
JB
VB
bB
hB
uB
{B
�B
�B
�B
�B
�B
%�B
-�B
4TB
:B
BuB
GzB
MB
TB
W�B
`B
e�B
kB
n�B
r�B
w2B
z^B
}B
.B
�UB
��11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111   B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B �B"�B$B$B$B%B%B%B%B$B%B%B$B#B �B�B�B�B�B �B �B �B!�B#B#B#B6tBX9B[SB^aBPBI�BD�BA�BB�BD�BE�BE�BB�BF�BF�BE�BE�BA�B7B4pB1XB.LB,>B+6B+8B).B' B&B$
B�B�B�B�B��B��B�tB�pB�hB�bB�]B�KB��B�kB�	B�B��B�nB��B�zBZnB" B�B
�4B
��B
ޖB
� B
��B
�/B
��B
�pB
v.B
h�B
Z�B
IB
5�B
*qB
3�B
+sB
1B
#EB
B
�B
�B
�B	�RB	�&B	��B	�]B	�B	��B	��B	��B	�tB	�<B	�B	�B	�B	�B	��B	��B	��B	|sB	xXB	kB	`�B	U�B	HAB	BB	:�B	/�B	+�B	(�B	$mB	2B	
�B�zB�:B�B��BצB�|B�MB�B��B��B��B��B��B��B�eB�QB�EB�2B�B��B��B��B��B��B��B��B��B~�B|�B{�By�BvlBumBs^BqPBpLBn=BnABqOBtfBvoBx~Bz�B}�B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B� B��B�BqSBm<Bm<BpOBpLBnABhBa�Bb�B_�BX�BO�BKuBF[BDOBA;B@3BA<BG`BJtBM�BN�BN�BL{BIjBL�BM�BL~BL~BL}BJuBKyBL~BJrBKyBL�BK|BLBO�BR�BU�B[�B\�B]�B_�Ba�Bi+BmEBoMBoOBnGBl:Bk4Bk4Bj1Bi(Bh%Bh#BoQBuoBw�Bx�Bx�B|�B{�B}�B�B��B��B�B�6B�ZB�qB��B��B��B��B��B��B��B��B��B��B��B�/B�TB�UB�XB�UB�`B�nBχBֶB��B��B��B�B�B�B�(B�2B�LB�]B�wB��B�B��B�tB�uB��B��B	�B	�B	�B	
�B		B	B	B	B	(B	KB	\B	 gB	$�B	%�B	'�B	+�B	5�B	<B	=B	>B	?B	@"B	@&B	A,B	A,B	D=B	LjB	U�B	X�B	Y�B	\�B	]�B	_�B	a�B	b�B	b�B	c�B	d�B	fB	fB	e�B	fB	iB	m.B	rMB	sQB	sTB	wiB	xqB	z{B	z}B	z{B	{�B	~�B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�+B	�EB	�_B	�pB	�vB	�zB	�zB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�5B	�4B	�?B	�MB	�[B	�^B	�iB	�sB	�tB	�|B	тB	ҁB	ԑB	ՙB	بB	ٰB	ۼB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�	B	�B	�B	�B	�B	�B	�(B	�,B	�&B	�6B	�9B	�?B	�AB	�9B	�CB	�?B	�JB	�GB	�OB	�TB	�^B	�eB	�pB	�sB	�qB	�|B	��B
�B
�B
�B
�B
�B
�B
�B
�G�O�B

B
�B
 �B
'�B
-XB
5�B
:�B
@_B
GUB
K B
SPB
Y?B
^`B
bB
f)B
jrB
m�B
pKB
rpB
t�B
w%11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111   <#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
G�O�<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
PRES            TEMP            PSAL            PRES_ADJUSTED = PRES - dP                                                                                                                                                                                                                                       none                                                                                                                                                                                                                                                            PSAL_ADJUSTED = PSAL + dS, where dS is calculated from a potential conductivity (ref to 0 dbar) multiplicative adjustment factor r.                                                                                                                             dP =0.04 dbar.                                                                                                                                                                                                                                                  none                                                                                                                                                                                                                                                            r =0.9997(+/-0), vertically averaged dS =-0.012(+/-0.002) in PSS-78.                                                                                                                                                                                            Pressures adjusted by using pressure offset at the sea surface. The quoted error is manufacturer specified accuracy in dbar.                                                                                                                                    The quoted error is manufacturer specified accuracy with respect to ITS-90 at time of laboratory calibration.                                                                                                                                                   Significant salinity drift detected. OWC least squares fit adopted. Map scales: x=6,3; y=2,1. Salinity also adjusted for effects of pressure adjustment. The quoted error is max[0.01, 1xOWC uncertainty] in PSS-78.                                            201906040940312019060409403120190604094031  AO  ARCAADJP                                                                    20171212010056    IP                G�O�G�O�G�O�                AO  ARGQQCPL                                                                    20171212010056  QCP$                G�O�G�O�G�O�5F03E           AO  ARGQQCPL                                                                    20171212010056  QCF$                G�O�G�O�G�O�0               UW  ARSQUWQC    WOD & nearby Argo as visual check                               20190604094031  IP                  G�O�G�O�G�O�                