CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             
   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       2018-11-21T04:11:52Z creation      
references        (http://www.argodatamgt.org/Documentation   comment       	free text      user_manual_version       3.2    Conventions       Argo-3.2 CF-1.6    featureType       trajectoryProfile      comment_dmqc_operator         (Matthew Alkire, University of Washington      @   	DATA_TYPE                  	long_name         	Data type      conventions       Argo reference table 1     
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
resolution        :�o     �  q�   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                    y�   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  {�   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                    �|   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  �|   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  0  �t   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                    ��   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                    ��   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                    ��   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  ,  ��   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                    ��   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                    ��   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                    ��   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                    ��   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  @  ��   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                    �    HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                    �0   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    �4   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar         �D   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar         �H   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�        �L   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    �PArgo profile    3.1 1.2 19500101000000  20181121041152  20190604094023  5903736 US ARGO PROJECT                                                 STEPHEN RISER                                                   PRES            TEMP            PSAL               �A   AO  4051                            2C  D   APEX                            5368                            041511                          846 @��=-��1   @��>]|��@2�n��O��d}`A�7L1   GPS     Primary sampling: mixed [deeper than nominal 985dbar: discrete; nominal 985dbar to surface: 2dbar-bin averaged]                                                                                                                                                    �A   B   B   @�ff@�  A   A   A@  A`  A�  A�  A�  A�  A�  A�  A�  A�  B   B  B  B  B   B(  B0  B8  B@  BH  BP  BX  B`ffBh  Bp  Bx  B�  B�  B�  B�  B�  B�33B�33B�  B�  B���B�  B�  B�  B�  B�  B�33B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�33B�  B�  B�  B�  B�  C   C  C  C  C  C
  C  C  C  C  C  C  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� DfD� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dt� Dt�fDy��D�RD�FfD���D��D�qD�@RD���D�ÅD�qD�6�D�|)D��fD��qD�<{Dڟ
D��D��D�/
D�qD���111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  @��@��R@��RA\)A?\)A_\)A\)A��A��A��A��AϮA߮A�A��B�
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
B`=pBg�
Bo�
Bw�
B�
B��B��B��B��B��B��B��B��B��RB��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��C��C��C��C��C	��C��C��C��C��C��C��C��C��C��C��C��C!��C#��C%��C'��C)��C+��C-��C/��C1��C3��C5��C7��C9��C;��C=��C?��CA��CC��CE��CG��CI��CK��CM��CO��CQ��CS��CU��CW��CY��C[��C]��C_��Ca��Cc��Ce��Cg��Ci��Ck��Cm��Co��Cq��Cs��Cu��Cw��Cy��C{��C}��C��C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���D }qD �qD}qD�qD}qD�qD}qD�qD}qD�qD}qD�qD}qD�qD}qD�qD}qD�qD	}qD	�qD
}qD
�qD}qD�D}qD�qD}qD�qD}qD�qD}qD�qD}qD�qD}qD�qD}qD�qD}qD�qD}qD�qD}qD�qD}qD�qD}qD�qD}qD�qD}qD�qD}qD�qD}qD�qD}qD�qD}qD�qD}qD�qD}qD�qD }qD �qD!}qD!�qD"}qD"�qD#}qD#�qD$}qD$�qD%}qD%�qD&}qD&�qD'}qD'�qD(}qD(�qD)}qD)�qD*}qD*�qD+}qD+�qD,}qD,�qD-}qD-�qD.}qD.�qD/}qD/�qD0}qD0�qD1}qD1�qD2}qD2�qD3}qD3�qD4}qD4�qD5}qD5�qD6}qD6�qD7}qD7�qD8}qD8�qD9}qD9�qD:}qD:�qD;}qD;�qD<}qD<�qD=}qD=�qD>}qD>�qD?}qD?�qD@}qD@�qDA}qDA�qDB}qDB�qDC}qDC�qDD}qDD�qDE}qDE�qDF}qDF�qDG}qDG�qDH}qDH�qDI}qDI�qDJ}qDJ�qDK}qDK�qDL}qDL�qDM}qDM�qDN}qDN�qDO}qDO�qDP}qDP�qDQ}qDQ�qDR}qDR�qDS}qDS�qDT}qDT�qDU}qDU�qDV}qDV�qDW}qDW�qDX}qDX�qDY}qDY�qDZ}qDZ�qD[}qD[�qD\}qD\�qD]}qD]�qD^}qD^�qD_}qD_�qD`}qD`�qDa}qDa�qDb}qDb�qDc}qDc�qDd}qDd�qDe}qDe�qDf}qDf�qDg}qDg�qDh}qDh�qDi}qDi�qDj}qDj�qDk}qDk�qDl}qDl�qDm}qDm�qDn}qDn�qDo}qDo�qDp}qDp�qDq}qDq�qDr}qDr�qDs}qDs�qDt}qDt��Dy� D�
D�ED��pD���D�)D�?
D���D��=D�)D�5pD�z�D��D��)D�;3Dڝ�D���D���D�-�D�)D��=111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  @��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��A�p�A�r�A�x�A�z�A�A�DA�PA�PA�PA�\A�\A�PA�\A�\A�\A�\A�hA�hA�uAᕁAᗍAᗍAᗍAᙚAᙚAᗍA�7A�p�A�G�A�5?A�ȴAް!AܓuA�
=A�VA���A�dZA��Aӣ�A�S�A�l�A�  AϼjAΧ�A˟�Aɇ+A�ĜA��
A�x�Aá�AÍPA�A���A�I�A��/A�A�A� �A�n�A�;dA���A��A�\)A�^5A�^5A��A�-A��9A���A�-A�x�A���A��`A�oA�{A�Q�A�r�A��hA��+A��jA�  A��A�33A��\A��wA�1'A��/A���A�(�A���A��A��A��A���A��A��9A���A���A��\A���A��+A��A��
A�jA�O�A���A��A��wA�t�A���A�ĜA�dZA��A��AA{p�Ay`BAw�PAv �AuAt��ArffAp$�Aml�Ae�AcoAb��Abn�A`�!A]��A[O�AY��AY�AX�yAX�`AXZAWl�AU��AS�
AR�/AR1'AQ�AP��AO7LAM�wAK��AJ�AI�AH��AG��AE�-AE33AC�AB�jAA�A@(�A?dZA>bNA=?}A<��A<��A<�!A;�-A9;dA7�;A5C�A3�A2�RA0��A.bNA,��A*z�A)A(VA'�FA&�A&��A&  A$=qA"��A!x�A�TA7LAQ�A
=AG�AI�AdZAz�A�;A7LA��A�;A?}A1AVA��A?}A�`AJA
��A	�A	l�A	VA��AJA��A��A
=A^5An�A  A ��A $�@�
=@��@�$�@��T@��D@�dZ@��H@�v�@�@��/@��w@�o@�M�@���@�|�@�+@��@�O�@�9X@�n�@���@�  @�E�@��@�z�@�(�@�\)@�-@�@���@�P@�-@�9@��y@�`B@۝�@��@�ȴ@�=q@٩�@ؼj@���@�o@�~�@�5?@�x�@ԛ�@�+@�@мj@�9X@Ο�@�O�@̓u@ʏ\@��@�/@���@�J@�$�@�{@���@őh@��`@�"�@�hs@��@��@��y@��@���@��7@�/@��@��/@�9X@�\)@���@��D@�  @�C�@��+@��h@��@��
@��@���@�/@���@��j@���@�r�@�Q�@��@�C�@���@�J@�x�@��j@���@��@�1'@��;@���@�o@�^5@�7L@�j@��@��
@�l�@��@��+@��@�`B@���@��-@���@�x�@�/@�j@�I�@�(�@�S�@��@�E�@��T@��@���@�hs@�G�@�V@��u@�Z@�9X@�  @�"�@��!@�n�@��#@���@�@���@��h@�G�@�7L@�V@��@���@�bN@�A�@�  @��@��H@�M�@��-@�/@��u@�b@��m@��w@���@��@�S�@�"�@���@�n�@�=q@�J@���@��^@��^@��-@�x�@��@���@��/@�Ĝ@��9@���@�z�@�A�@��;@�dZ@�@���@��R@�n�@�=q@�5?@��T@���@��@���@�Z@�Q�@�Q�@�I�@�A�@�1'@�1'@��@��w@���@�dZ@�+@��@���@��R@�^5@�M�@�^5@�^5@�-@��-@�p�@�&�@��u@�1'@�(�@��m@�K�@�"�@��@���@�V@�E�@�$�@��@��#@��-@��h@�/@��@���@�r�@��@��F@�"�@���@��@��@�o@���@�K�@�n�@�$�@�{@���@�/@�bN@�9X@� �@�1@��m@��@���@�\)@��@���@���@�@��7@�/@��@��@��u@� �@��
@��F@���@�|�@�+@�
=@��H@���@�k�@y�M@q��@i�=@a<6@V�@M�Z@F�1@@j@9�@3\)@,��@%@ _@��@c�@��@b�@r�@	�@Z�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111  A�p�A�r�A�x�A�z�A�A�DA�PA�PA�PA�\A�\A�PA�\A�\A�\A�\A�hA�hA�uAᕁAᗍAᗍAᗍAᙚAᙚAᗍA�7A�p�A�G�A�5?A�ȴAް!AܓuA�
=A�VA���A�dZA��Aӣ�A�S�A�l�A�  AϼjAΧ�A˟�Aɇ+A�ĜA��
A�x�Aá�AÍPA�A���A�I�A��/A�A�A� �A�n�A�;dA���A��A�\)A�^5A�^5A��A�-A��9A���A�-A�x�A���A��`A�oA�{A�Q�A�r�A��hA��+A��jA�  A��A�33A��\A��wA�1'A��/A���A�(�A���A��A��A��A���A��A��9A���A���A��\A���A��+A��A��
A�jA�O�A���A��A��wA�t�A���A�ĜA�dZA��A��AA{p�Ay`BAw�PAv �AuAt��ArffAp$�Aml�Ae�AcoAb��Abn�A`�!A]��A[O�AY��AY�AX�yAX�`AXZAWl�AU��AS�
AR�/AR1'AQ�AP��AO7LAM�wAK��AJ�AI�AH��AG��AE�-AE33AC�AB�jAA�A@(�A?dZA>bNA=?}A<��A<��A<�!A;�-A9;dA7�;A5C�A3�A2�RA0��A.bNA,��A*z�A)A(VA'�FA&�A&��A&  A$=qA"��A!x�A�TA7LAQ�A
=AG�AI�AdZAz�A�;A7LA��A�;A?}A1AVA��A?}A�`AJA
��A	�A	l�A	VA��AJA��A��A
=A^5An�A  A ��A $�@�
=@��@�$�@��T@��D@�dZ@��H@�v�@�@��/@��w@�o@�M�@���@�|�@�+@��@�O�@�9X@�n�@���@�  @�E�@��@�z�@�(�@�\)@�-@�@���@�P@�-@�9@��y@�`B@۝�@��@�ȴ@�=q@٩�@ؼj@���@�o@�~�@�5?@�x�@ԛ�@�+@�@мj@�9X@Ο�@�O�@̓u@ʏ\@��@�/@���@�J@�$�@�{@���@őh@��`@�"�@�hs@��@��@��y@��@���@��7@�/@��@��/@�9X@�\)@���@��D@�  @�C�@��+@��h@��@��
@��@���@�/@���@��j@���@�r�@�Q�@��@�C�@���@�J@�x�@��j@���@��@�1'@��;@���@�o@�^5@�7L@�j@��@��
@�l�@��@��+@��@�`B@���@��-@���@�x�@�/@�j@�I�@�(�@�S�@��@�E�@��T@��@���@�hs@�G�@�V@��u@�Z@�9X@�  @�"�@��!@�n�@��#@���@�@���@��h@�G�@�7L@�V@��@���@�bN@�A�@�  @��@��H@�M�@��-@�/@��u@�b@��m@��w@���@��@�S�@�"�@���@�n�@�=q@�J@���@��^@��^@��-@�x�@��@���@��/@�Ĝ@��9@���@�z�@�A�@��;@�dZ@�@���@��R@�n�@�=q@�5?@��T@���@��@���@�Z@�Q�@�Q�@�I�@�A�@�1'@�1'@��@��w@���@�dZ@�+@��@���@��R@�^5@�M�@�^5@�^5@�-@��-@�p�@�&�@��u@�1'@�(�@��m@�K�@�"�@��@���@�V@�E�@�$�@��@��#@��-@��h@�/@��@���@�r�@��@��F@�"�@���@��@��@�o@���@�K�@�n�@�$�@�{@���@�/@�bN@�9X@� �@�1@��m@��@���@�\)@��@���@���@�@��7@�/@��@��@��u@� �@��
@��F@���@�|�@�+@�
=@��HG�O�@�k�@y�M@q��@i�=@a<6@V�@M�Z@F�1@@j@9�@3\)@,��@%@ _@��@c�@��@b�@r�@	�@Z�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111  ;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oG�O�;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oBoBoBuBuB{B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�BhB\BB
�B
ɺB�B�
B�HB�{B]/BK�BT�B��B��B6FB}�B�1Bq�BW
BE�B+BuBN�Bk�BgmB��B�B��B��B�qB��B�B�5B�5B�HB�B�B�B�B�B�sB�TB�/B�
B��B��BǮB�}B�!B��B��B�hB�Bx�Bq�BhsBYBK�BB�B8RB2-B �BB��B�BɺB�^B�B��B��B�bB�DB�bB�+Bo�BZBL�BA�B7LB/B�B
��B
�B
��B
��B
�B
ffB
S�B
E�B
:^B
5?B
-B
�B
B	�yB	�^B	��B	��B	��B	�oB	� B	r�B	l�B	l�B	l�B	k�B	ffB	ffB	dZB	`BB	\)B	XB	R�B	N�B	H�B	A�B	9XB	33B	-B	'�B	 �B	�B	{B	VB		7B	B��B��B��B�B�B�B�B�mB�;B�B��BɺBƨB��B�XB�9B�'B�B�B�B�B�B�B��B��B��B��B��B��B��B�{B�PB�7B�1B�+B�B�B�B~�B{�By�Bu�Br�Bp�Bo�Bm�Bn�Bn�Bl�BgmBbNB^5B\)B\)B]/BgmBm�Bk�Bq�Bs�By�B}�B�B�B�%B�%B�+B�7B�oB�{B��B��B��B��B��B��B��B��B��B��B��B��B�'B�?B�?B�FB�jBĜBŢBĜBB��B�}B�qB�dB�dB�dB�qB�jB�qB�wB��B��BBĜBÖBÖBŢBǮBȴB��B��B��B��B��B��B��B��B�B�
B�
B�
B�
B��B�B�B�B�)B�5B�;B�BB�fB�B�B�B��B	B	JB	VB	PB	PB	VB	JB	JB	\B	uB	{B	{B	{B	�B	�B	�B	�B	�B	�B	�B	!�B	$�B	$�B	$�B	&�B	'�B	(�B	)�B	,B	0!B	7LB	:^B	<jB	?}B	B�B	B�B	C�B	F�B	H�B	J�B	L�B	L�B	N�B	Q�B	R�B	S�B	XB	ZB	]/B	`BB	cTB	e`B	ffB	hsB	jB	l�B	n�B	o�B	p�B	s�B	t�B	u�B	x�B	y�B	z�B	{�B	~�B	�B	�B	�B	�B	�B	�+B	�7B	�DB	�VB	�oB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�'B	�-B	�9B	�?B	�LB	�LB	�LB	�RB	�XB	�jB	�jB	�qB	�wB	�wB	�wB	�}B	��B	ĜB	ƨB	ƨB	ƨB	ƨB	ǮB	ɺB	ɺB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�
B	�B	�B	�B	�B	�B	�B	�#B	�/B	�/B	�5B	�BB	�HB	�HB	�TB	�ZB	�ZB	�ZB	�fB	�fB	�mB	�sB	�sB	�sB	�yB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
B
B
%B
+B
	7B

=B
JB
JB
JB
PB
PB
PB
VB
VB
\B
\B
\B
bB
bB
hB
�B
�B
�B
%zB
,�B
5�B
A�B
IRB
O(B
T�B
Z�B
]�B
bNB
iyB
m]B
r�B
w�B
z^B
}B
��B
��B
�t111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111  BkBlBqBpBuB|B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�BcBXB B
�B
ƼB�B��B�@B�pBZ(BH�BQ�B��B��B38Bz�B�&Bn�BTBB�B'�BjBK�Bh{BdeB��B��B��B��B�bBȻB�B�.B�+B�:B�xB�|B�B�zB�wB�jB�HB�!B��B��B��BĤB�tB�B��B��B�[BBu�Bn�BefBVBH�B?�B5IB/!B�B B�B�BƷB�_B�B��B��B�]B�CB�`B�+Bl�BWBI�B>�B4IB,B�B
��B
�B
��B
��B
�B
cmB
P�B
B�B
7gB
2FB
*B
�B
%B	�B	�jB	��B	��B	��B	�|B	}B	o�B	i�B	i�B	i�B	h�B	crB	cuB	afB	]NB	Y7B	UB	O�B	K�B	E�B	>�B	6hB	0BB	*B	$�B	�B	�B	�B	cB	GB	+B�B��B��B��B�B�B�B�}B�JB�%B��B��BúB��B�hB�KB�;B�,B�&B�(B�(B�'B�B�B�B��B��B��B��B��B��B�fB�LB�EB�?B�0B�-B~B|Bx�Bv�Br�Bo�Bm�Bl�Bj�Bk�Bk�Bi�Bd�B_gB[LBY@BY?BZFBd�Bj�Bh�Bn�Bp�Bv�B{B~B"B�=B�8B�@B�KB��B��B��B��B��B��B��B��B��B��B��B��B��B��B�;B�WB�TB�[B��B��B·B��B��B��B��B��B�xB�sB�{B��B��B��B��B��B��B��B��B��B��B·BĿB��B��B��B��B��B�B�B�B�B�B�B�B�B�B�B�B�%B�*B�@B�JB�QB�RB�zB�B�B�B��B	,B		^B	kB	
eB	
fB	iB		^B		]B	pB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	!�B	!�B	!�B	#�B	%B	&	B	'B	)B	-2B	4aB	7qB	9�B	<�B	?�B	?�B	@�B	C�B	E�B	G�B	I�B	I�B	K�B	N�B	PB	QB	U"B	W0B	ZCB	]WB	`jB	btB	cyB	e�B	g�B	i�B	k�B	l�B	m�B	p�B	q�B	r�B	u�B	v�B	w�B	x�B	|B	B	 B	�&B	�-B	�/B	�>B	�JB	�WB	�hB	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�8B	�@B	�IB	�PB	�]B	�[B	�[B	�bB	�hB	�{B	�{B	��B	��B	��B	��B	��B	��B	��B	úB	øB	ùB	ùB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�	B	�B	�B	�!B	�B	�)B	�%B	�$B	�/B	�5B	�@B	�?B	�FB	�RB	�WB	�YB	�cB	�lB	�lB	�jB	�xB	�tB	�{B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	�B	��B	��B	�B	��B	��B	��B	��B	��B	��B	�B	�B	��B	��B	��B	��B	��B	��B	��B	�B
'B
5B
:B
IB
LB
	[B
	[B
	XB

]B

]B

^B
dB
eB
nB
kB
nB
pB
nB
tG�O�B
B
�B
"�B
)�B
2�B
>�B
F_B
L5B
Q�B
W�B
Z�B
_aB
f�B
jkB
o�B
t�B
wmB
zB
}�B
��B
��111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111  <#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
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
PRES            TEMP            PSAL            PRES_ADJUSTED = PRES - dP                                                                                                                                                                                                                                       none                                                                                                                                                                                                                                                            PSAL_ADJUSTED = PSAL + dS, where dS is calculated from a potential conductivity (ref to 0 dbar) multiplicative adjustment factor r.                                                                                                                             dP =0.04 dbar.                                                                                                                                                                                                                                                  none                                                                                                                                                                                                                                                            r =0.9999(+/-0), vertically averaged dS =-0.003(+/-0.002) in PSS-78.                                                                                                                                                                                            Pressures adjusted by using pressure offset at the sea surface. The quoted error is manufacturer specified accuracy in dbar.                                                                                                                                    The quoted error is manufacturer specified accuracy with respect to ITS-90 at time of laboratory calibration.                                                                                                                                                   Significant salinity drift detected. OWC least squares fit adopted. Map scales: x=6,3; y=2,1. Salinity also adjusted for effects of pressure adjustment. The quoted error is max[0.01, 1xOWC uncertainty] in PSS-78.                                            201906040940232019060409402320190604094023  AO  ARCAADJP                                                                    20181121041152    IP                G�O�G�O�G�O�                AO  ARGQQCPL                                                                    20181121041152  QCP$                G�O�G�O�G�O�5F03E           AO  ARGQQCPL                                                                    20181121041152  QCF$                G�O�G�O�G�O�0               UW  ARSQUWQC    WOD & nearby Argo as visual check                               20190604094023  IP                  G�O�G�O�G�O�                