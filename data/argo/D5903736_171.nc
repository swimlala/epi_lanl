CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             
   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       2018-11-21T04:11:55Z creation      
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
_FillValue                    �,Argo profile    3.1 1.2 19500101000000  20181121041155  20190604094025  5903736 US ARGO PROJECT                                                 STEPHEN RISER                                                   PRES            TEMP            PSAL               �A   AO  4051                            2C  D   APEX                            5368                            041511                          846 @��o�<�1   @��pB���@4&�-�d�I�^1   GPS     Primary sampling: mixed [deeper than nominal 985dbar: discrete; nominal 985dbar to surface: 2dbar-bin averaged]                                                                                                                                                    �A   B   B   @�ff@�  A   A   A@  A`  A�  A�  A�  A�  A�  A�  A�  A�  B   B  B  B  B   B(  B0  B8  B@  BH  BP  BX  B`  Bh  Bp  Bx  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�33B�  B�  B�  B�  B�  B�  C   C  C  C  C  C
  C  C  C  C  C  C  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� DfD� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK�fDLfDL�fDM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DRfDR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dt� Dt��Dy��D�
D�=�D��=D��
D��D�>fD�eD���D�qD�<�D��D���D���D�2�DڅD�ɚD�HD�?
D�b=11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   @��@��R@��RA\)A?\)A_\)A\)A��A��A��A��AϮA߮A�A��B�
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
B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��C��C��C��C��C	��C��C��C��C��C��C��C��C��C��C��C��C!��C#��C%��C'��C)��C+��C-��C/��C1��C3��C5��C7��C9��C;��C=��C?��CA��CC��CE��CG��CI��CK��CM��CO��CQ��CS��CU��CW��CY��C[��C]��C_��Ca��Cc��Ce��Cg��Ci��Ck��Cm��Co��Cq��Cs��Cu��Cw��Cy��C{��C}��C��C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C��C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���D }qD �qD}qD�qD}qD�qD}qD�qD}qD�qD}qD�qD}qD�qD}qD�D}qD�qD	}qD	�qD
}qD
�qD}qD�qD}qD�qD}qD�qD}qD�qD}qD�qD}qD�qD}qD�qD}qD�qD}qD�qD}qD�qD}qD�qD}qD�qD}qD�qD}qD�qD}qD�qD}qD�qD}qD�qD}qD�qD}qD�qD}qD�qD}qD�qD }qD �qD!}qD!�qD"}qD"�qD#}qD#�qD$}qD$�qD%}qD%�qD&}qD&�qD'}qD'�qD(}qD(�qD)}qD)�qD*}qD*�qD+}qD+�qD,}qD,�qD-}qD-�qD.}qD.�qD/}qD/�qD0}qD0�qD1}qD1�qD2}qD2�qD3}qD3�qD4}qD4�qD5}qD5�qD6}qD6�qD7}qD7�qD8}qD8�qD9}qD9�qD:}qD:�qD;}qD;�qD<}qD<�qD=}qD=�qD>}qD>�qD?}qD?�qD@}qD@�qDA}qDA�qDB}qDB�qDC}qDC�qDD}qDD�qDE}qDE�qDF}qDF�qDG}qDG�qDH}qDH�qDI}qDI�qDJ}qDJ�qDK��DL�DL��DL�qDM}qDM�qDN}qDN�qDO}qDO�qDP}qDP�qDQ}qDR�DR}qDR�qDS}qDS�qDT}qDT�qDU}qDU�qDV}qDV�qDW}qDW�qDX}qDX�qDY}qDY�qDZ}qDZ�qD[}qD[�qD\}qD\�qD]}qD]�qD^}qD^�qD_}qD_�qD`}qD`�qDa}qDa�qDb}qDb�qDc}qDc�qDd}qDd�qDe}qDe�qDf}qDf�qDg}qDg�qDh}qDh�qDi}qDi�qDj}qDj�qDk}qDk�qDl}qDl�qDm}qDm�qDn}qDn�qDo}qDo�qDp}qDp�qDq}qDq�qDr}qDr�qDs}qDs�qDt}qDt�>Dy�D��D�<{D���D���D�GD�=D�c�D�׮D�)D�;�D���D��RD��fD�1�Dڃ�D��RD� D�=�D�`�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   @��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��A���A��
A��
A��
A��A��#A��#A��;A��HA��HA��HA��TA��TA��mA��`A��yA��yA��A��A��A��A��A��A��A��A��A��A��A��A��A���A���A�bA�K�A���A��A��A�?}A�|�Aϡ�Aϴ9AϺ^Aϕ�A��A��AΙ�A�v�A�O�A�A�S�A̟�A˼jA�\)Aɏ\A��yA�hsA�Q�AŮA�M�A7A��A�v�A�
=A��FA�hsA�z�A�"�A�hsA��mA��PA��A�K�A��wA�Q�A��yA���A�=qA�(�A�G�A��A���A�r�A��A���A�S�A�O�A��A�ȴA��
A��-A�Q�A�jA���A��A�oA�9XA�$�A�9XA�|�A�G�A��A��A�A�A��A�ȴA�I�A���A�"�A�ffA�(�A�O�A���A��^A�r�A�(�A��A�Q�A���A���A��A���A�v�A�\)A�G�A��A~5?Az�/Ay�PAv�At�jAt~�As�7Ar-AqG�Ap$�Am�Ak%Ajv�Ai|�Ahv�Ae�AdQ�Ac�AcC�Ab�Ab^5Aa/A]ƨA\�A[?}AZ^5AZ$�AY"�AW�^AWl�AWVAV~�AT��ATASƨAS�wAS��ASC�AR~�AQ�wAP�9AN�uANv�AM�AKAFI�AE��AE�AC�;A@��A>��A<M�A8n�A5�FA4Q�A3/A1`BA/�A.1A+�^A*^5A)��A(VA&�jA$�DA#��A"�A"JA!��A!��A!dZA �AdZA�A�A��AVA �A\)A�A�;A�A�A��AC�A��A�A �Ax�A%AZA�A��A	\)A��AȴA1'A��A�DA-A;dA�AoA �u@��@��P@�J@�1@�J@��@�33@�@���@�ƨ@�@��@�ȴ@�  @��`@�
=@�Q�@�\)@��@�?}@���@�|�@��@�X@ܼj@�9X@ۥ�@�33@��y@ڸR@���@��`@�j@��;@�S�@�n�@��@�l�@�A�@�S�@�M�@́@���@̼j@ʏ\@�@�V@��@�%@�A�@�1@ǶF@Ƈ+@��`@ēu@� �@å�@�@�ff@�@��D@�dZ@���@��T@�`B@��@�\)@�{@���@��@��/@�V@��/@�Ĝ@��@��u@�Q�@�1@��F@�C�@��R@�7L@��j@�A�@��@��P@�@�p�@��j@�j@��@��@�E�@�$�@�-@�-@���@��@��^@���@��w@�;d@�"�@���@�v�@�J@��@���@�;d@�M�@��@���@��@��T@��@��@���@�O�@�&�@��@���@�bN@��m@�K�@�;d@��@���@�ff@��#@���@���@���@��#@��T@��T@��T@��T@��#@��T@��@���@�@��@���@�?}@���@���@�j@�I�@�(�@� �@��@�  @��@��
@�ƨ@���@�|�@�l�@�l�@�dZ@�dZ@�\)@�S�@�C�@��@��@��;@��
@��F@��F@�dZ@�@��@��@��@��H@�ȴ@���@���@���@���@�E�@���@�V@��@��j@���@��j@���@��D@�z�@��D@�z�@�I�@�1@���@�dZ@��@��R@�J@���@��@���@��F@�C�@��H@���@�V@�G�@���@�r�@�Z@�1'@��m@��P@�S�@�K�@�+@���@���@�v�@�V@�$�@�J@��@��-@�p�@�?}@�&�@��@��@���@���@��@�bN@�Z@�A�@�(�@�b@�1@��@��
@���@�|�@�;d@�~�@�{@���@�@�@�@��-@�X@��@��@���@�Ĝ@��@�K^@���@|u�@uX@l�/@i�7@aw2@X �@Q�Z@IA @@��@9ϫ@3��@,��@&�s@!^�@Z�@Ta@GE@�!11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111411111111111111111111   A���A��
A��
A��
A��A��#A��#A��;A��HA��HA��HA��TA��TA��mA��`A��yA��yA��A��A��A��A��A��A��A��A��A��A��A��A��A���A���A�bA�K�A���A��A��A�?}A�|�Aϡ�Aϴ9AϺ^Aϕ�A��A��AΙ�A�v�A�O�A�A�S�A̟�A˼jA�\)Aɏ\A��yA�hsA�Q�AŮA�M�A7A��A�v�A�
=A��FA�hsA�z�A�"�A�hsA��mA��PA��A�K�A��wA�Q�A��yA���A�=qA�(�A�G�A��A���A�r�A��A���A�S�A�O�A��A�ȴA��
A��-A�Q�A�jA���A��A�oA�9XA�$�A�9XA�|�A�G�A��A��A�A�A��A�ȴA�I�A���A�"�A�ffA�(�A�O�A���A��^A�r�A�(�A��A�Q�A���A���A��A���A�v�A�\)A�G�A��A~5?Az�/Ay�PAv�At�jAt~�As�7Ar-AqG�Ap$�Am�Ak%Ajv�Ai|�Ahv�Ae�AdQ�Ac�AcC�Ab�Ab^5Aa/A]ƨA\�A[?}AZ^5AZ$�AY"�AW�^AWl�AWVAV~�AT��ATASƨAS�wAS��ASC�AR~�AQ�wAP�9AN�uANv�AM�AKAFI�AE��AE�AC�;A@��A>��A<M�A8n�A5�FA4Q�A3/A1`BA/�A.1A+�^A*^5A)��A(VA&�jA$�DA#��A"�A"JA!��A!��A!dZA �AdZA�A�A��AVA �A\)A�A�;A�A�A��AC�A��A�A �Ax�A%AZA�A��A	\)A��AȴA1'A��A�DA-A;dA�AoA �u@��@��P@�J@�1@�J@��@�33@�@���@�ƨ@�@��@�ȴ@�  @��`@�
=@�Q�@�\)@��@�?}@���@�|�@��@�X@ܼj@�9X@ۥ�@�33@��y@ڸR@���@��`@�j@��;@�S�@�n�@��@�l�@�A�@�S�@�M�@́@���@̼j@ʏ\@�@�V@��@�%@�A�@�1@ǶF@Ƈ+@��`@ēu@� �@å�@�@�ff@�@��D@�dZ@���@��T@�`B@��@�\)@�{@���@��@��/@�V@��/@�Ĝ@��@��u@�Q�@�1@��F@�C�@��R@�7L@��j@�A�@��@��P@�@�p�@��j@�j@��@��@�E�@�$�@�-@�-@���@��@��^@���@��w@�;d@�"�@���@�v�@�J@��@���@�;d@�M�@��@���@��@��T@��@��@���@�O�@�&�@��@���@�bN@��m@�K�@�;d@��@���@�ff@��#@���@���@���@��#@��T@��T@��T@��T@��#@��T@��@���@�@��@���@�?}@���@���@�j@�I�@�(�@� �@��@�  @��@��
@�ƨ@���@�|�@�l�@�l�@�dZ@�dZ@�\)@�S�@�C�@��@��@��;@��
@��F@��F@�dZ@�@��@��@��@��H@�ȴ@���@���@���@���@�E�@���@�V@��@��j@���@��j@���@��D@�z�@��D@�z�@�I�@�1@���@�dZ@��@��R@�J@���@��@���@��F@�C�@��H@���@�V@�G�@���@�r�@�Z@�1'@��m@��P@�S�@�K�@�+@���@���@�v�@�V@�$�@�J@��@��-@�p�@�?}@�&�@��@��@���@���@��@�bN@�Z@�A�@�(�@�b@�1@��@��
@���@�|�@�;d@�~�@�{@���@�@�@�@��-@�X@��@��@���@�ĜG�O�@�K^@���@|u�@uX@l�/@i�7@aw2@X �@Q�Z@IA @@��@9ϫ@3��@,��@&�s@!^�@Z�@Ta@GE@�!11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111411111111111111111111   ;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oG�O�;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oB>wB>wB>wB>wB>wB>wB>wB>wB=qB>wB=qB=qB>wB?}B?}B?}B?}B?}B@�B@�B@�B@�BA�B@�B?}B>wB?}B?}B>wB?}B@�BC�BL�BiyB��B�LB��B�HB1B#�B6FBW
Be`Bp�Bu�By�Bz�Bz�Bz�B� B�B}�BgmB_;B7LB^5B�B�VB�Bt�Bn�Bk�B\)B?}B�B�B1'B$�B �BgmB�%B�B�DB�\B��B��B��B�\B�Bz�Bp�BgmBZBD�B9XB�BDB��B�ZB�dB�jBȴBɺBŢBǮB�qB��B��Bq�BZBT�B_;B}�B�Bn�BQ�B-B�B+B2-B0!B'�B"�B$�B �B
��B
�B
�yB
�
B
ƨB
�jB
�RB
�?B
�'B
��B
�PB
� B
iyB
_;B
L�B
<jB
9XB
2-B
)�B
"�B
�B
	7B	��B	��B	�B	�fB	��B	��B	ǮB	ĜB	B	�jB	�9B	��B	��B	�hB	�JB	�=B	�B	}�B	{�B	x�B	t�B	k�B	hsB	ffB	ffB	e`B	bNB	^5B	YB	R�B	H�B	G�B	A�B	2-B	�B	�B	oB		7B��B�B�fB�)B�B�
B��B��B��B��BŢB��B�qB�qB�dB�?B�FBÖBȴBȴBȴBɺBɺBǮBŢBƨBƨBŢBŢBƨBŢBƨBĜB�}B�qB�^B�LB�RB�RB�XB�^B�^B�LB�'B�B�B��B��B��B��B�hB�\B�\B�\B�JB�7B�1B�%B�B�B�B�B�7B�DB�=B�DB�PB�\B�JB�+B�B�B�B�B�B�B�B�B�7B�=B�DB�JB�PB�\B�\B�bB�bB�bB�hB��B��B��B��B��B��B��B��B��B�9B�dB�^B�RB�RB�^B��B��B��BŢB��B��B��B��B�B�B�B�#B�NB�`B�fB�mB�sB�B�B��B��B	  B		7B	JB	JB	PB	PB	PB	VB	VB	\B	VB	\B	bB	hB	hB	uB	uB	oB	oB	uB	uB	�B	�B	 �B	 �B	#�B	"�B	%�B	&�B	+B	,B	+B	+B	+B	,B	-B	0!B	1'B	2-B	5?B	7LB	9XB	;dB	>wB	>wB	?}B	?}B	@�B	A�B	B�B	D�B	G�B	I�B	M�B	N�B	P�B	Q�B	Q�B	T�B	W
B	XB	ZB	\)B	_;B	`BB	dZB	hsB	jB	r�B	v�B	w�B	y�B	}�B	�B	�B	�1B	�7B	�=B	�DB	�JB	�PB	�PB	�VB	�\B	�hB	�hB	�uB	��B	��B	��B	��B	��B	��B	��B	��B	�B	�FB	�RB	�XB	�XB	�dB	�dB	�qB	�}B	��B	B	ÖB	ǮB	ȴB	ɺB	��B	��B	��B	ɺB	ȴB	ɺB	��B	��B	��B	��B	�B	�B	�B	�)B	�/B	�;B	�BB	�HB	�HB	�TB	�ZB	�ZB	�`B	�fB	�TB	�NB	�HB	�BB	�;B	�5B	�;B	�BB	�BB	�BB	�HB	�NB	�TB	�TB	�ZB	�`B	�mB	�yB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
  B
oB
�B
1B
 B
)�B
2�B
8�B
?�B
E�B
M�B
S�B
X�B
]~B
b�B
gB
mCB
s�B
x�B
}"B
�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111411111111111111111111   B8�B8�B8�B8�B8�B8�B8�B8�B7�B8�B7�B7�B8�B9�B9�B9�B9�B9�B:�B:�B:�B:�B<B:�B9�B8�B9�B9�B8�B9�B;B>BGFBc�B�aB��B�DBۻB�BGB0�BQyB_�BkBp1BtJBuNBuNBuKBzhB}|Bx_Ba�BY�B1�BX�B{sB��B~�Bo.BiBe�BV�B9�B�B�B+�BQB8Ba�B��B�B��B��B�B�B��B��B~�BuRBkBa�BT�B?B3�BB�B�lB��B��B��B�/B�5B�B�'B��B�qB�Bl+BT�BO{BY�BxvB�BiBLkB'�B9B%�B,�B*�B"nBSB`BDB
��B
�B
��B
ѐB
�1B
��B
��B
��B
��B
�ZB
��B
z�B
dB
Y�B
G]B
6�B
3�B
,�B
$�B
cB
*B
�B	�vB	�\B	�0B	��B	ΌB	�aB	�AB	�/B	�%B	��B	��B	�cB	�&B	�B	��B	��B	~�B	x�B	v~B	spB	oUB	fB	cB	aB	aB	_�B	\�B	X�B	S�B	M�B	CKB	BGB	<#B	,�B	EB	"B	B	�B�sB�DB�B��BҲBѪBΜBˇB�zB�cB�HB�*B�B�B�
B��B��B�8B�TB�VB�XB�]B�]B�TB�EB�HB�HB�CB�HB�JB�HB�MB�CB�(B�B� B��B��B��B��B�B�B��B��B��B��B��B�B�kB�OB�B�B�B�B��B��B��B��B�B}�B{�B}�B��B��B��B��B��B�B��B��B�B~�B~�B~�B|�B{�B|�B�B��B��B��B��B��B�B�B�B�B�
B�B�BB�iB�mB�aB�^B�hB�\B�dB��B��B�B�B��B��B�B�-B�,B�3B�JB�uB�zBʆBΝBҹBҸB��B��B��B�	B�B�B�B�CB�PB�_B�yB��B	�B	�B	�B	�B	�B	�B	�B	�B	
 B	�B	
B		B	B	B	B	B	B	B	B	B	1B	\B	kB	lB	~B	uB	 �B	!�B	%�B	&�B	%�B	%�B	%�B	&�B	'�B	*�B	+�B	,�B	/�B	1�B	3�B	6B	9B	9B	:B	:!B	;(B	<.B	=3B	??B	BPB	DYB	HuB	I~B	K�B	L�B	L�B	O�B	Q�B	R�B	T�B	V�B	Y�B	Z�B	^�B	cB	e"B	mQB	qpB	rqB	t{B	x�B	|�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�
B	�B	�(B	�6B	�6B	�;B	�BB	�EB	�[B	�oB	��B	��B	��B	��B	��B	�B	�B	�B	�B	�"B	�-B	�5B	�OB	�VB	�ZB	�dB	�^B	�`B	�\B	�UB	�]B	�_B	̋B	ΚB	ϟB	ТB	СB	ӴB	��B	��B	��B	��B	��B	��B	��B	��B	��B	� B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�
B	�B	�B	�#B	�)B	�+B	�2B	�6B	�:B	�<B	�@B	�AB	�EB	�?B	�JB	�LB	�LB	�JB	�MB	�OB	�KB	�NB	�TB	�TB	�UB	�RB	�cB	�fB	�oB	�kB	�sB	�rB	�sB	�|B	�~B	�~B	��B	��G�O�B	�B
	@B
�B
�B
$`B
-jB
3#B
:LB
@�B
HB
NDB
S.B
XB
]SB
a�B
g�B
nkB
s�B
w�B
z�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111411111111111111111111   <#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
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
PRES            TEMP            PSAL            PRES_ADJUSTED = PRES - dP                                                                                                                                                                                                                                       none                                                                                                                                                                                                                                                            PSAL_ADJUSTED = PSAL + dS, where dS is calculated from a potential conductivity (ref to 0 dbar) multiplicative adjustment factor r.                                                                                                                             dP =0.04 dbar.                                                                                                                                                                                                                                                  none                                                                                                                                                                                                                                                            r =0.9999(+/-0), vertically averaged dS =-0.005(+/-0.001) in PSS-78.                                                                                                                                                                                            Pressures adjusted by using pressure offset at the sea surface. The quoted error is manufacturer specified accuracy in dbar.                                                                                                                                    The quoted error is manufacturer specified accuracy with respect to ITS-90 at time of laboratory calibration.                                                                                                                                                   Significant salinity drift detected. OWC least squares fit adopted. Map scales: x=6,3; y=2,1. Salinity also adjusted for effects of pressure adjustment. The quoted error is max[0.01, 1xOWC uncertainty] in PSS-78.                                            201906040940252019060409402520190604094025  AO  ARCAADJP                                                                    20181121041155    IP                G�O�G�O�G�O�                AO  ARGQQCPL                                                                    20181121041155  QCP$                G�O�G�O�G�O�5F03E           AO  ARGQQCPL                                                                    20181121041155  QCF$                G�O�G�O�G�O�0               UW  ARSQUWQC    WOD & nearby Argo as visual check                               20190604094025  IP                  G�O�G�O�G�O�                