CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       2018-10-24T14:15:43Z creation      
references        (http://www.argodatamgt.org/Documentation   comment       	free text      user_manual_version       3.2    Conventions       Argo-3.2 CF-1.6    featureType       trajectoryProfile         @   	DATA_TYPE                  	long_name         	Data type      conventions       Argo reference table 1     
_FillValue                    6�   FORMAT_VERSION                 	long_name         File format version    
_FillValue                    6�   HANDBOOK_VERSION               	long_name         Data handbook version      
_FillValue                    6�   REFERENCE_DATE_TIME                 	long_name         !Date of reference for Julian days      conventions       YYYYMMDDHHMISS     
_FillValue                    6�   DATE_CREATION                   	long_name         Date of file creation      conventions       YYYYMMDDHHMISS     
_FillValue                    6�   DATE_UPDATE                 	long_name         Date of update of this file    conventions       YYYYMMDDHHMISS     
_FillValue                    6�   PLATFORM_NUMBER                   	long_name         Float unique identifier    conventions       WMO float identifier : A9IIIII     
_FillValue                    6�   PROJECT_NAME                  	long_name         Name of the project    
_FillValue                  @  6�   PI_NAME                   	long_name         "Name of the principal investigator     
_FillValue                  @  7(   STATION_PARAMETERS           	            	long_name         ,List of available parameters for the station   conventions       Argo reference table 3     
_FillValue                  0  7h   CYCLE_NUMBER               	long_name         Float cycle number     conventions       =0...N, 0 : launch cycle (if exists), 1 : first complete cycle      
_FillValue         ��        7�   	DIRECTION                  	long_name         !Direction of the station profiles      conventions       -A: ascending profiles, D: descending profiles      
_FillValue                    7�   DATA_CENTRE                   	long_name         .Data centre in charge of float data processing     conventions       Argo reference table 4     
_FillValue                    7�   DC_REFERENCE                  	long_name         (Station unique identifier in data centre   conventions       Data centre convention     
_FillValue                     7�   DATA_STATE_INDICATOR                  	long_name         1Degree of processing the data have passed through      conventions       Argo reference table 6     
_FillValue                    7�   	DATA_MODE                  	long_name         Delayed mode or real time data     conventions       >R : real time; D : delayed mode; A : real time with adjustment     
_FillValue                    7�   PLATFORM_TYPE                     	long_name         Type of float      conventions       Argo reference table 23    
_FillValue                     7�   FLOAT_SERIAL_NO                   	long_name         Serial number of the float     
_FillValue                     7�   FIRMWARE_VERSION                  	long_name         Instrument firmware version    
_FillValue                     8   WMO_INST_TYPE                     	long_name         Coded instrument type      conventions       Argo reference table 8     
_FillValue                    8,   JULD               	long_name         ?Julian day (UTC) of the station relative to REFERENCE_DATE_TIME    standard_name         time   units         "days since 1950-01-01 00:00:00 UTC     conventions       8Relative julian days with decimal part (as parts of day)   
resolution        >�EȠ      
_FillValue        A.�~       axis      T           80   JULD_QC                	long_name         Quality on date and time   conventions       Argo reference table 2     
_FillValue                    88   JULD_LOCATION                  	long_name         @Julian day (UTC) of the location relative to REFERENCE_DATE_TIME   units         "days since 1950-01-01 00:00:00 UTC     conventions       8Relative julian days with decimal part (as parts of day)   
resolution        >�EȠ      
_FillValue        A.�~            8<   LATITUDE               	long_name         &Latitude of the station, best estimate     standard_name         latitude   units         degree_north   
_FillValue        @�i�       	valid_min         �V�        	valid_max         @V�        axis      Y           8D   	LONGITUDE                  	long_name         'Longitude of the station, best estimate    standard_name         	longitude      units         degree_east    
_FillValue        @�i�       	valid_min         �f�        	valid_max         @f�        axis      X           8L   POSITION_QC                	long_name         ,Quality on position (latitude and longitude)   conventions       Argo reference table 2     
_FillValue                    8T   POSITIONING_SYSTEM                    	long_name         Positioning system     
_FillValue                    8X   VERTICAL_SAMPLING_SCHEME                  	long_name         Vertical sampling scheme   conventions       Argo reference table 16    
_FillValue                    8`   CONFIG_MISSION_NUMBER                  	long_name         :Unique number denoting the missions performed by the float     conventions       !1...N, 1 : first complete mission      
_FillValue         ��        9`   PROFILE_PRES_QC                	long_name         #Global quality flag of PRES profile    conventions       Argo reference table 2a    
_FillValue                    9d   PROFILE_TEMP_QC                	long_name         #Global quality flag of TEMP profile    conventions       Argo reference table 2a    
_FillValue                    9h   PROFILE_PSAL_QC                	long_name         #Global quality flag of PSAL profile    conventions       Argo reference table 2a    
_FillValue                    9l   PRES         
      
   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���   axis      Z        �  9p   PRES_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  A8   PRES_ADJUSTED            
      	   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  C,   PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  J�   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         decibar    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  L�   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  T�   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  \x   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  ^l   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  f4   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         degree_Celsius     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  h(   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  o�   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  w�   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  y�   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �t   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  �h   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  0  �0   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                    �`   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                    �`   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                    �`   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  ,  �`   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                    ��   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                    ��   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                    ��   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                    ��   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  @  ��   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                    ��   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                    ��   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    ��   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar         �    HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar         �   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�        �   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    �Argo profile    3.1 1.2 19500101000000  20181024141543  20181024141543  5904970 US ARGO PROJECT                                                 CARL SZCZECHOWSKI                                               PRES            TEMP            PSAL               �A   AO  6785                            2B  A   APEX                            7726                            111215                          846 @���w(s1   @���#`@6���vȴ�d����1   GPS     Primary sampling: discrete []                                                                                                                                                                                                                                      �A   A   A   @@  @�  @�  A   A   AA��A`  A�  A�  A�  A�  A�  A�33A�  A�  B   BffB  B  B   B(  B0  B8  B@  BH  BP  BX  B`  Bh  Bp  Bx  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B���B���B�  B�  B�  B�  B�  B���B�  B�  B�  B�  B�  B�  B�  C   C  C  C  C  C
  C  C  C  C  C  C  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF�CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf�Ch�Cj  Ck�fCm�fCp  Cr  Ct  Cv�Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  Dy�D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"y�D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D*��D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN�fDO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`fD`� Da  Da� Db  Db�fDc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dt� Du  Du� Dv  Dv� Dw  Dw� Dy�
D�?�D��f111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  @=p�@}p�@��R@��RA\)A@��A_\)A\)A��A��A��A��A��GA߮A�A��B=pB�
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
B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��BøRBǸRB��B��B��B��B��B߸RB��B��B��B��B��B��B��B��C��C��C��C��C	��C��C��C��C��C��C��C��C��C��C��C��C!��C#��C%��C'��C)��C+��C-��C/��C1��C3��C5��C7��C9��C;��C=��C?��CA��CC��CF]CG��CI��CK��CM��CO��CQ��CS��CU��CW��CY��C[��C]��C_��Ca��Cc��Cf]Ch]Ci��Ck�)Cm�)Co��Cq��Cs��Cv]Cw��Cy��C{��C}��C��C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C��C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���D }qD �qD}qD�qD}qD�qD}qD�qD}qD�qD}qD�qD}qD�qDwD�qD}qD�qD	}qD	�qD
}qD
�qD}qD�qD}qD�qD}qD�qD}qD�qD}qD�qD}qD�qD}qD�qD}qD�qD}qD�qD}qD�qD}qD�qD}qD�qD}qD�qD}qD�qD}qD�qD}qD�qD}qD�qD}qD�qD}qD�qD}qD�qD}qD�qD }qD �qD!}qD!�qD"wD"�qD#}qD#�qD$}qD$�qD%}qD%�qD&}qD&�qD'}qD'�qD(}qD(�qD)}qD)�qD*}qD*�D+}qD+�qD,}qD,�qD-}qD-�qD.}qD.�qD/}qD/�qD0}qD0�qD1}qD1�qD2}qD2�qD3}qD3�qD4}qD4�qD5}qD5�qD6}qD6�qD7}qD7�qD8}qD8�qD9}qD9�qD:}qD:�qD;}qD;�qD<}qD<�qD=}qD=�qD>}qD>�qD?}qD?�qD@}qD@�qDA}qDA�qDB}qDB�qDC}qDC�qDD}qDD�qDE}qDE�qDF}qDF�qDG}qDG�qDH}qDH�qDI}qDI�qDJ}qDJ�qDK}qDK�qDL}qDL�qDM}qDM�qDN��DN�qDO}qDO�qDP}qDP�qDQ}qDQ�qDR}qDR�qDS}qDS�qDT}qDT�qDU}qDU�qDV}qDV�qDW}qDW�qDX}qDX�qDY}qDY�qDZ}qDZ�qD[}qD[�qD\}qD\�qD]}qD]�qD^}qD^�qD_}qD`�D`}qD`�qDa}qDa�qDb��Db�qDc}qDc�qDd}qDd�qDe}qDe�qDf}qDf�qDg}qDg�qDh}qDh�qDi}qDi�qDj}qDj�qDk}qDk�qDl}qDl�qDm}qDm�qDn}qDn�qDo}qDo�qDp}qDp�qDq}qDq�qDr}qDr�qDs}qDs�qDt}qDt�qDu}qDu�qDv}qDv�qDw}qDy�{D�>fD��111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�A�/A�"�A��A��A��A��A�{A�1A���A���A���A���A���A�  A�A�A�A�A�A�A�  A�  A���A���A��A��TA���A��FA���A��DA��A�v�A�?}A�/A��A��A�+A�;dA�33A�VA���A�\)A���A�M�A�A�A�E�A�A�A�5?A�33A�(�A��A�K�A��TA�ƨA�5?A�z�A���A��yA�7LA��#A�O�A�x�A��hA���A��HA�r�A��uA���A�+A��A�VA���A�;dA�G�A���A�O�A�
=A��A���A�A�ffA���A���A�S�A�  A�;dA�K�A���A�K�A��7A�(�A�JA��jA�-A�bNA�/A�x�A�C�A�33A���A��A��A��A��!A��9A���A�;dA��A�"�A�-A��A~Q�A}C�Az��Ay"�Aw�7Au�Atz�Ar�ArbAq&�Ap�Ao|�Ao�An��Am��AljAk�mAk�-Aj��Aj(�AiƨAh�Ag��AgdZAfjAd�/Ac�FAb~�AaC�A`�+A`{A_�A_�wA^�yA]`BA\$�AZ  AY�hAYhsAY�AXffAW�PAV��AT�AS��AR�HAP�uANQ�AM�AL1AKG�AJ�AI��AHv�AEƨAD-AA�wA>v�A<�RA;�;A:n�A9ƨA9XA8�DA7|�A6��A61A5A3�-A3A2A0Q�A.�uA-�^A, �A*�uA)&�A(1'A&��A&�\A&n�A&�A%XA$�\A#��A#%A!��A A�A�\At�AffA�A�`AZAJA�FAXA9XA�RAAȴA�A��A�A?}A��A^5A&�AA"�A�RAI�AƨAhsA
�A
 �A	��A	A�PA�A��A��A��A��A��A  A��A%A ff@�33@��P@�V@��/@�ȴ@���@�-@�l�@���@���@� �@�{@�t�@��@�9X@��y@��T@���@���@��#@�hs@��`@܃@ܴ9@�`B@ݲ-@�\)@�I�@�hs@⟾@�-@��#@�@���@���@ߝ�@��/@��@�-@��/@׮@ӍP@Ѻ^@�"�@���@�j@�C�@�O�@�ƨ@��^@�x�@�Q�@�1@�@���@�X@�p�@�z�@� �@��
@�dZ@�;d@�@���@��7@��7@�X@�Ĝ@��/@�j@��@���@�o@��H@�V@�@�v�@�5?@�@���@���@��7@�hs@���@��j@���@�(�@� �@�A�@���@��F@�ƨ@�;d@�l�@�ȴ@�5?@��^@�Ĝ@�1'@�o@��R@�n�@�{@��@�$�@��@��@�J@���@�?}@���@��@�l�@���@�ȴ@��@�^5@��7@���@���@�5?@���@�bN@��`@��h@��@��!@�\)@���@��^@��@��-@�O�@��@��@��;@�l�@�C�@��9@��^@��@���@���@�dZ@���@�9X@��-@�&�@�M�@�v�@�ȴ@���@�n�@�=q@�-@��@��/@�1'@�ƨ@��@�E�@���@�b@�|�@��@��`@�?}@��@�t�@�|�@�33@�J@��@�ƨ@��@�l�@�K�@�;d@��@��F@���@���@�p�@�`B@��@�j@�|�@��H@�v�@�@���@��@��`@���@�bN@�bN@��
@��m@��@��H@���@�dZ@�j@�1'@�S�@��R@�v�@�@��T@���@�&�@�&�@��@��`@���@�Ĝ@��9@��@�Q�@� �@��
@�|�@�o@��!@��!@���@���@�~�@�ff@�5?@��@�@���@��@���@��@�Q�@�I�@� �@��;@��P@�33@�@��@�ȴ@���@�~�@�v�@�n�@�ff@�^5@�M�@���@�@��^@�\)@y	l@gC�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  A�/A�"�A��A��A��A��A�{A�1A���A���A���A���A���A�  A�A�A�A�A�A�A�  A�  A���A���A��A��TA���A��FA���A��DA��A�v�A�?}A�/A��A��A�+A�;dA�33A�VA���A�\)A���A�M�A�A�A�E�A�A�A�5?A�33A�(�A��A�K�A��TA�ƨA�5?A�z�A���A��yA�7LA��#A�O�A�x�A��hA���A��HA�r�A��uA���A�+A��A�VA���A�;dA�G�A���A�O�A�
=A��A���A�A�ffA���A���A�S�A�  A�;dA�K�A���A�K�A��7A�(�A�JA��jA�-A�bNA�/A�x�A�C�A�33A���A��A��A��A��!A��9A���A�;dA��A�"�A�-A��A~Q�A}C�Az��Ay"�Aw�7Au�Atz�Ar�ArbAq&�Ap�Ao|�Ao�An��Am��AljAk�mAk�-Aj��Aj(�AiƨAh�Ag��AgdZAfjAd�/Ac�FAb~�AaC�A`�+A`{A_�A_�wA^�yA]`BA\$�AZ  AY�hAYhsAY�AXffAW�PAV��AT�AS��AR�HAP�uANQ�AM�AL1AKG�AJ�AI��AHv�AEƨAD-AA�wA>v�A<�RA;�;A:n�A9ƨA9XA8�DA7|�A6��A61A5A3�-A3A2A0Q�A.�uA-�^A, �A*�uA)&�A(1'A&��A&�\A&n�A&�A%XA$�\A#��A#%A!��A A�A�\At�AffA�A�`AZAJA�FAXA9XA�RAAȴA�A��A�A?}A��A^5A&�AA"�A�RAI�AƨAhsA
�A
 �A	��A	A�PA�A��A��A��A��A��A  A��A%A ff@�33@��P@�V@��/@�ȴ@���@�-@�l�@���@���@� �@�{@�t�@��@�9X@��y@��T@���@���@��#@�hs@��`@܃@ܴ9@�`B@ݲ-@�\)@�I�@�hs@⟾@�-@��#@�@���@���@ߝ�@��/@��@�-@��/@׮@ӍP@Ѻ^@�"�@���@�j@�C�@�O�@�ƨ@��^@�x�@�Q�@�1@�@���@�X@�p�@�z�@� �@��
@�dZ@�;d@�@���@��7@��7@�X@�Ĝ@��/@�j@��@���@�o@��H@�V@�@�v�@�5?@�@���@���@��7@�hs@���@��j@���@�(�@� �@�A�@���@��F@�ƨ@�;d@�l�@�ȴ@�5?@��^@�Ĝ@�1'@�o@��R@�n�@�{@��@�$�@��@��@�J@���@�?}@���@��@�l�@���@�ȴ@��@�^5@��7@���@���@�5?@���@�bN@��`@��h@��@��!@�\)@���@��^@��@��-@�O�@��@��@��;@�l�@�C�@��9@��^@��@���@���@�dZ@���@�9X@��-@�&�@�M�@�v�@�ȴ@���@�n�@�=q@�-@��@��/@�1'@�ƨ@��@�E�@���@�b@�|�@��@��`@�?}@��@�t�@�|�@�33@�J@��@�ƨ@��@�l�@�K�@�;d@��@��F@���@���@�p�@�`B@��@�j@�|�@��H@�v�@�@���@��@��`@���@�bN@�bN@��
@��m@��@��H@���@�dZ@�j@�1'@�S�@��R@�v�@�@��T@���@�&�@�&�@��@��`@���@�Ĝ@��9@��@�Q�@� �@��
@�|�@�o@��!@��!@���@���@�~�@�ff@�5?@��@�@���@��@���@��@�Q�@�I�@� �@��;@��P@�33@�@��@�ȴ@���@�~�@�v�@�n�@�ff@�^5@�M�@���@�@��^@�\)@y	l@gC�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�B_;BdZBhsBiyBjBjBm�Bt�Bx�By�By�Bz�B{�B|�B�B�B�%B�+B�1B�+B�=B�=B�DB�PB�\B�bB�uB�{B��B��B��B��B��B��B��B��B��B��B�B�B�B�!B�3B��B�B�5B�HB�NB�ZB�ZB�TB�ZB�)B�B�B�B��B��B��B��B��B��BĜB�B��B�PB� Bq�Bn�Bn�BcTBgmBe`BW
BT�BM�B:^B#�BPB  B��B�yB�/B�#B�HB�5B�#B��B��BȴB�}B�B��B��B��B�uB�%B� Br�BiyB]/B=qB �B�B
��B
�HB
��B
ŢB
�wB
�?B
��B
��B
�JB
� B
o�B
gmB
YB
O�B
C�B
<jB
7LB
/B
)�B
&�B
!�B
�B
�B
\B
VB
	7B
B
  B	��B	��B	�B	�yB	�;B	��B	��B	B	�jB	�XB	�LB	�FB	�!B	��B	��B	�uB	�PB	�JB	�=B	�+B	� B	y�B	n�B	hsB	`BB	R�B	B�B	<jB	6FB	2-B	.B	)�B	$�B	�B	DB	B�B�mB�NB�5B�B�B��B��B��B��BƨBB�wB�^B�3B�B��B��B��B��B��B��B�{B�uB�oB�\B�DB�VB�PB�%B�Bv�Bq�Bn�Bl�BjBhsBgmBe`BcTBcTB]/B[#BYBW
BVBT�BS�BR�BP�BN�BM�BG�BF�BG�BI�BI�BH�BI�BI�BH�BJ�BH�BH�BF�BD�BC�BB�B@�B?}B>wB>wB>wB;dB7LB6FB5?B5?B6FB6FB8RB8RB9XB9XB5?B/B-B-B-B,B-B,B,B-B.B5?B>wBE�BVBe`Bw�B�=B�uB��B��B��B��B��B��B� B|�B��B��B��B��B�uB�JB�B�B}�B|�B{�B�B�B�1B�%B�%B�1B�7B�1B�7B�PB�uB�{B��B��B��B��B��B��B��B��B��B��B��B�B�B��B�B�B�FB�^B�^B�^B�dB�jBBɺB��B��B�B�B�;B�B�B��B��B��B	B	B	B	B	B	B	B	+B	
=B	DB	VB	\B	hB	oB	{B	uB	{B	�B	�B	�B	�B	�B	�B	�B	{B	oB	�B	�B	(�B	-B	%�B	.B	+B	-B	33B	49B	5?B	6FB	7LB	9XB	;dB	;dB	E�B	N�B	R�B	R�B	Q�B	M�B	R�B	VB	bNB	]/B	e`B	ffB	k�B	m�B	m�B	l�B	l�B	m�B	n�B	l�B	jB	iyB	hsB	dZB	cTB	bNB	l�B	o�B	t�B	q�B	p�B	s�B	u�B	v�B	u�B	x�B	x�B	x�B	x�B	x�B	y�B	~�B	z�B	y�B	y�B	y�B	x�B	x�B	x�B	x�B	y�B	y�B	~�B	�uB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�'B	�-B	�3B	�-B	�'B	�3B	�?B	�FB	�?B	�FB	�RB	�XB	�dB	�jB	�qB	��B	��B	��B	B	B	B	ĜB	ƨB	ȴB	ɺB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�
B	�B	�)B	�)B	�)B	�)B	�)B	�)B	�)B	�5B	�5B	�5B	��B
 �B
M111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  B_;BdZBhsBiyBjBjBm�Bt�Bx�By�By�Bz�B{�B|�B�B�B�%B�+B�1B�+B�=B�=B�DB�PB�\B�bB�uB�{B��B��B��B��B��B��B��B��B��B��B�B�B�B�!B�3B��B�B�5B�HB�NB�ZB�ZB�TB�ZB�)B�B�B�B��B��B��B��B��B��BĜB�B��B�PB� Bq�Bn�Bn�BcTBgmBe`BW
BT�BM�B:^B#�BPB  B��B�yB�/B�#B�HB�5B�#B��B��BȴB�}B�B��B��B��B�uB�%B� Br�BiyB]/B=qB �B�B
��B
�HB
��B
ŢB
�wB
�?B
��B
��B
�JB
� B
o�B
gmB
YB
O�B
C�B
<jB
7LB
/B
)�B
&�B
!�B
�B
�B
\B
VB
	7B
B
  B	��B	��B	�B	�yB	�;B	��B	��B	B	�jB	�XB	�LB	�FB	�!B	��B	��B	�uB	�PB	�JB	�=B	�+B	� B	y�B	n�B	hsB	`BB	R�B	B�B	<jB	6FB	2-B	.B	)�B	$�B	�B	DB	B�B�mB�NB�5B�B�B��B��B��B��BƨBB�wB�^B�3B�B��B��B��B��B��B��B�{B�uB�oB�\B�DB�VB�PB�%B�Bv�Bq�Bn�Bl�BjBhsBgmBe`BcTBcTB]/B[#BYBW
BVBT�BS�BR�BP�BN�BM�BG�BF�BG�BI�BI�BH�BI�BI�BH�BJ�BH�BH�BF�BD�BC�BB�B@�B?}B>wB>wB>wB;dB7LB6FB5?B5?B6FB6FB8RB8RB9XB9XB5?B/B-B-B-B,B-B,B,B-B.B5?B>wBE�BVBe`Bw�B�=B�uB��B��B��B��B��B��B� B|�B��B��B��B��B�uB�JB�B�B}�B|�B{�B�B�B�1B�%B�%B�1B�7B�1B�7B�PB�uB�{B��B��B��B��B��B��B��B��B��B��B��B�B�B��B�B�B�FB�^B�^B�^B�dB�jBBɺB��B��B�B�B�;B�B�B��B��B��B	B	B	B	B	B	B	B	+B	
=B	DB	VB	\B	hB	oB	{B	uB	{B	�B	�B	�B	�B	�B	�B	�B	{B	oB	�B	�B	(�B	-B	%�B	.B	+B	-B	33B	49B	5?B	6FB	7LB	9XB	;dB	;dB	E�B	N�B	R�B	R�B	Q�B	M�B	R�B	VB	bNB	]/B	e`B	ffB	k�B	m�B	m�B	l�B	l�B	m�B	n�B	l�B	jB	iyB	hsB	dZB	cTB	bNB	l�B	o�B	t�B	q�B	p�B	s�B	u�B	v�B	u�B	x�B	x�B	x�B	x�B	x�B	y�B	~�B	z�B	y�B	y�B	y�B	x�B	x�B	x�B	x�B	y�B	y�B	~�B	�uB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�'B	�-B	�3B	�-B	�'B	�3B	�?B	�FB	�?B	�FB	�RB	�XB	�dB	�jB	�qB	��B	��B	��B	B	B	B	ĜB	ƨB	ȴB	ɺB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�
B	�B	�)B	�)B	�)B	�)B	�)B	�)B	�)B	�5B	�5B	�5B	��B
 �B
M111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�PRES            TEMP            PSAL            PRES_ADJUSTED = PRES - surface_pressure                                                                                                                                                                                                                         none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            surface_pressure=0.04 dbar                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                      Pressure adjusted at real time based on most recent valid surface pressure                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                      20181024141543                              AO  ARCAADJP                                                                    20181024141543    IP                G�O�G�O�G�O�                AO  ARGQQCPL                                                                    20181024141543  QCP$                G�O�G�O�G�O�5F03E           AO  ARGQQCPL                                                                    20181024141543  QCF$                G�O�G�O�G�O�0               