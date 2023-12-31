CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             
   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       2018-11-21T12:58:26Z creation      
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
_FillValue                    ��   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  ��   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  0  ��   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                    ��   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                    ��   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                    ��   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  ,  ��   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                    ��   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                    ��   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                    ��   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                    �    HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  @  �   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                    �D   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                    �T   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    �X   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar         �h   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar         �l   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�        �p   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    �tArgo profile    3.1 1.2 19500101000000  20181121125826  20190408133245  5904461 US ARGO PROJECT                                                 STEPHEN RISER                                                   PRES            TEMP            PSAL               �A   AO  5286                            2C  D   APEX                            6531                            072314                          846 @��bY�{�1   @��b�� `@5d�t�j�cA�7K�1   GPS     Primary sampling: mixed [deeper than nominal 985dbar: discrete; nominal 985dbar to surface: 2dbar-bin averaged]                                                                                                                                                    �A   B   B   @9��@�  @�  A   A!��A@  A`  A�  A�  A�  A�  A�  A�  A�  A�  B   B  B  B  B   B(  B0  B8  B@  BH  BP  BX  B`ffBh  Bp  Bx  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  C   C  C  C  C  C
  C  C  C  C  C  C  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ�C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D��D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dt� Dt�fDyY�D�3D�L�D�|�D�� D��D�33D���D��fD�fD�FfD�vfD���D�fD�)�Dڌ�D���D��D�P D�3D�� 1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 @7
>@}p�@��R@��RA ��A?\)A_\)A\)A��A��A��A��AϮA߮A�A��B�
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
B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��C��C��C��C��C	��C��C��C��C��C��C��C��C��C��C��C��C!��C#��C%��C'��C)��C+��C-��C/��C1��C3��C5��C7��C9��C;��C=��C?��CA��CC��CE��CG��CI��CK��CM��CO��CQ��CS��CU��CW��CZ]C[��C]��C_��Ca��Cc��Ce��Cg��Ci��Ck��Cm��Co��Cq��Cs��Cu��Cw��Cy��C{��C}��C��C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���D }qD �qD}qD�qD}qD�qD}qD�qD}qD�qD}qD�qD}qD�qD}qD�qD}qD�qD	}qD	�qD
}qD
�qD}qD�qD}qD�qD}qD�qD}qD�qD}qD�qD}qD�qD}qD�qD}qD�qD}qD�qD}qD�qD}qD�qD}qD�qD}qD�qD}qD�qD}qD�qD}qD�qD}qD�qD}qD�qD}qD�D}qD�qD}qD�qD }qD �qD!}qD!�qD"}qD"�qD#}qD#�qD$}qD$�qD%}qD%�qD&}qD&�qD'}qD'�qD(}qD(�qD)}qD)�qD*}qD*�qD+}qD+�qD,}qD,�qD-}qD-�qD.}qD.�qD/}qD/�qD0}qD0�qD1}qD1�qD2}qD2�qD3}qD3�qD4}qD4�qD5}qD5�qD6}qD6�qD7}qD7�qD8}qD8�qD9}qD9�qD:}qD:�qD;}qD;�qD<}qD<�qD=}qD=�qD>}qD>�qD?}qD?�qD@}qD@�qDA}qDA�qDB}qDB�qDC}qDC�qDD}qDD�qDE}qDE�qDF}qDF�qDG}qDG�qDH}qDH�qDI}qDI�qDJ}qDJ�qDK}qDK�qDL}qDL�qDM}qDM�qDN}qDN�qDO}qDO�qDP}qDP�qDQ}qDQ�qDR}qDR�qDS}qDS�qDT}qDT�qDU}qDU�qDV}qDV�qDW}qDW�qDX}qDX�qDY}qDY�qDZ}qDZ�qD[}qD[�qD\}qD\�qD]}qD]�qD^}qD^�qD_}qD_�qD`}qD`�qDa}qDa�qDb}qDb�qDc}qDc�qDd}qDd�qDe}qDe�qDf}qDf�qDg}qDg�qDh}qDh�qDi}qDi�qDj}qDj�qDk}qDk�qDl}qDl�qDm}qDm�qDn}qDn�qDo}qDo�qDp}qDp�qDq}qDq�qDr}qDr�qDs}qDs�qDt}qDt��DyWD��D�K�D�{�D�~�D��D�1�D��RD��D�D�ED�uD�ۅD�D�(RDڋ�D�˅D��D�N�D��D���1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 @��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��A��;A��/A��#A��#A��/A��/A��/A��;A��;A��`A��yA��A��A��TA���AΥ�A�hsA��A�33A�
=A�\)Aȧ�A�E�A�&�A�VA��A��#A�Q�A��A��Aƺ^AƓuAƍPA�bNA��yA�{A��A��9A��A��^A���A�VA���A���A��-A��;A�~�A�ȴA��7A��A�S�A��FA��A��A�G�A��A�^5A��PA��hA���A�VA��HA�t�A�-A�9XA�jA�VA���A���A��A�hsA���A��PA��RA�ĜA�M�A�r�A��
A���A�1'A�~�A���A�VA��A��A���A��uA�ƨA�^5A�^5A�`BA�l�A��A���A�n�A���A�v�A���A�hsA���A�ĜA� �A���A��^A�A�A�ffA���A��A��DAdZA|9XAx�+Au��AtI�Ap�Al�AiK�Agx�AeVAc��Ab�AadZA_�FA]��A[x�AY�mAY;dAY
=AXĜAXVAW�AT��AS
=AQ"�AN��AL�AK�hAJ~�AGoADM�AC+AA�PA=��A:�HA9S�A7x�A6n�A5�A4�+A2��A1�
A0��A/VA-�A+�;A*A�A)O�A(��A(�DA'+A%�#A%�A"��A!&�A ��A v�A 1'AƨA`BA��A�wA�A�A7LA5?A�A�PA��At�A�A�A�`A^5A��AĜAVA�PA�`A�+A-AO�A
^5A	|�A�/A��A�A�RA�#A�AXA=qAhsA�RAƨA&�A ��A I�@�|�@���@���@�z�@��@��@��@��@�(�@�K�@�hs@�9X@�o@�~�@��@홚@�7@�9@�~�@��#@��@�w@���@�=q@��#@�7L@䛦@�I�@�ƨ@�+@��@�z�@�S�@�r�@��@ם�@�K�@ְ!@��m@���@�@և+@�@��@��@ҏ\@��/@ύP@�ƨ@�33@���@��@�@Гu@�z�@�\)@��y@Ϯ@Ϯ@�I�@ЋD@�j@�Q�@�(�@���@ύP@�~�@�X@��`@̋D@̼j@��`@�V@�Ĝ@�b@���@��m@�C�@��@��@Ɵ�@Ƨ�@Ə\@�n�@�ff@ư!@��y@�dZ@ǥ�@�@Ə\@�$�@�Ĝ@ċD@�t�@��@��@��H@��y@��@�?}@�Ĝ@��;@�t�@��H@���@��@�p�@��D@�ƨ@�
=@��\@��@��@��@�ȴ@��@��-@�@��7@�O�@�%@��/@���@���@�&�@�7L@��/@�z�@�(�@��@�@�@�$�@���@���@��D@�b@��F@�dZ@��@��y@�^5@�@��@��#@���@���@�X@��u@�Z@�1@�t�@��y@�ȴ@�n�@�^5@�$�@��@��7@�V@��9@�r�@��
@��@��H@�ff@�"�@�t�@�"�@��!@�^5@���@�G�@�%@�A�@���@�o@���@��@��\@�^5@�J@���@�/@��@�Ĝ@�z�@�9X@��P@��@��@���@��+@��+@�^5@��@��^@�x�@���@� �@�  @��@��@���@���@�S�@��y@���@�M�@��T@�@���@���@���@�O�@�/@�O�@�X@�`B@�?}@�A�@���@��
@��@��m@��@���@�=q@���@���@��#@���@��^@��^@���@�`B@�Ĝ@�Q�@���@���@�33@�o@�S�@�C�@��@���@��@���@�5?@��@��-@�hs@�/@�Ĝ@�b@��F@�dZ@��@��@�33@�+@�+@��@��@�"�@�
=@�ȴ@�ff@��@���@��@��@�?}@�/@�%@���@�r�@�1'@��;@���@�K�@���@��j@�1@�1'@w�;@n��@e�@]��@V{@N�+@HQ�@@Q�@9X@4�@/�w@*n�@%�T@�y@��@��@�@��1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111 A��;A��/A��#A��#A��/A��/A��/A��;A��;A��`A��yA��A��A��TA���AΥ�A�hsA��A�33A�
=A�\)Aȧ�A�E�A�&�A�VA��A��#A�Q�A��A��Aƺ^AƓuAƍPA�bNA��yA�{A��A��9A��A��^A���A�VA���A���A��-A��;A�~�A�ȴA��7A��A�S�A��FA��A��A�G�A��A�^5A��PA��hA���A�VA��HA�t�A�-A�9XA�jA�VA���A���A��A�hsA���A��PA��RA�ĜA�M�A�r�A��
A���A�1'A�~�A���A�VA��A��A���A��uA�ƨA�^5A�^5A�`BA�l�A��A���A�n�A���A�v�A���A�hsA���A�ĜA� �A���A��^A�A�A�ffA���A��A��DAdZA|9XAx�+Au��AtI�Ap�Al�AiK�Agx�AeVAc��Ab�AadZA_�FA]��A[x�AY�mAY;dAY
=AXĜAXVAW�AT��AS
=AQ"�AN��AL�AK�hAJ~�AGoADM�AC+AA�PA=��A:�HA9S�A7x�A6n�A5�A4�+A2��A1�
A0��A/VA-�A+�;A*A�A)O�A(��A(�DA'+A%�#A%�A"��A!&�A ��A v�A 1'AƨA`BA��A�wA�A�A7LA5?A�A�PA��At�A�A�A�`A^5A��AĜAVA�PA�`A�+A-AO�A
^5A	|�A�/A��A�A�RA�#A�AXA=qAhsA�RAƨA&�A ��A I�@�|�@���@���@�z�@��@��@��@��@�(�@�K�@�hs@�9X@�o@�~�@��@홚@�7@�9@�~�@��#@��@�w@���@�=q@��#@�7L@䛦@�I�@�ƨ@�+@��@�z�@�S�@�r�@��@ם�@�K�@ְ!@��m@���@�@և+@�@��@��@ҏ\@��/@ύP@�ƨ@�33@���@��@�@Гu@�z�@�\)@��y@Ϯ@Ϯ@�I�@ЋD@�j@�Q�@�(�@���@ύP@�~�@�X@��`@̋D@̼j@��`@�V@�Ĝ@�b@���@��m@�C�@��@��@Ɵ�@Ƨ�@Ə\@�n�@�ff@ư!@��y@�dZ@ǥ�@�@Ə\@�$�@�Ĝ@ċD@�t�@��@��@��H@��y@��@�?}@�Ĝ@��;@�t�@��H@���@��@�p�@��D@�ƨ@�
=@��\@��@��@��@�ȴ@��@��-@�@��7@�O�@�%@��/@���@���@�&�@�7L@��/@�z�@�(�@��@�@�@�$�@���@���@��D@�b@��F@�dZ@��@��y@�^5@�@��@��#@���@���@�X@��u@�Z@�1@�t�@��y@�ȴ@�n�@�^5@�$�@��@��7@�V@��9@�r�@��
@��@��H@�ff@�"�@�t�@�"�@��!@�^5@���@�G�@�%@�A�@���@�o@���@��@��\@�^5@�J@���@�/@��@�Ĝ@�z�@�9X@��P@��@��@���@��+@��+@�^5@��@��^@�x�@���@� �@�  @��@��@���@���@�S�@��y@���@�M�@��T@�@���@���@���@�O�@�/@�O�@�X@�`B@�?}@�A�@���@��
@��@��m@��@���@�=q@���@���@��#@���@��^@��^@���@�`B@�Ĝ@�Q�@���@���@�33@�o@�S�@�C�@��@���@��@���@�5?@��@��-@�hs@�/@�Ĝ@�b@��F@�dZ@��@��@�33@�+@�+@��@��@�"�@�
=@�ȴ@�ff@��@���@��@��@�?}@�/@�%@���@�r�@�1'@��;@���@�K�G�O�@��j@�1@�1'@w�;@n��@e�@]��@V{@N�+@HQ�@@Q�@9X@4�@/�w@*n�@%�T@�y@��@��@�@��1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111 ;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oG�O�;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oB��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B�B�B�-B�B��B�B�LBÖBɺB��B��B��B�B�HB�BB�HB�fB�sB�B%B)�BVBw�B�B�B�%B�%B�+B��B��B�XB�BB�BB�mB��B��B��B��B��B��B��B�B�mB��B�}B�dBǮB�B�B��BÖB��BȴB��B�LB��B��B�B��B�{B�=By�Bl�B]/BQ�BA�B-B)�BhB	7B��B�NB��B��B��B�B�BǮB��B� Br�B^5BE�B!�B
��B
ŢB
��B
ffB
E�B
<jB
/B
'�B
�B

=B
JB	��B	�mB	�
B	ȴB	�3B	��B	~�B	p�B	_;B	VB	N�B	F�B	<jB	1'B	%�B	�B	�B	�B	�B	uB	DB	  B��B�B�yB�;B�/B�B��BB�dB�FB�!B��B��B��B��B��B�{B�VB�7B�By�Bx�Bw�Bt�Bs�Br�Bp�Bq�Bm�Bl�BiyBhsBgmBffBffBgmBhsBjBiyBcTB_;B]/B[#B\)B^5BZB^5BcTBdZBbNBdZBdZBe`BdZBe`BhsBl�Bp�Bp�Bs�Bt�Bs�Bu�Bu�Bu�Bu�Bx�B~�B�B�B�B�B�B�+B�JB�JB�\B�VB�JB�=B�B� B}�B}�B|�Bz�Bx�Bx�By�B{�B|�B~�B�B�B�B�B�1B�7B�7B�7B�=B�DB�bB�oB�hB�oB�uB�uB�hB�1B�B�B�+B��B��B��B�B�!B�'B��B�LB�9B�RB�}B��B��BȴB��B�B�B�B�B��B	  B	JB	\B	hB	oB	uB	{B	�B	�B	�B	{B	�B	�B	 �B	"�B	$�B	%�B	'�B	+B	+B	%�B	�B	 �B	#�B	$�B	'�B	,B	0!B	2-B	7LB	:^B	E�B	J�B	J�B	F�B	G�B	H�B	H�B	K�B	Q�B	S�B	VB	XB	XB	XB	XB	[#B	^5B	_;B	`BB	`BB	_;B	bNB	cTB	cTB	e`B	hsB	e`B	ffB	hsB	jB	jB	k�B	l�B	n�B	p�B	q�B	r�B	t�B	v�B	y�B	{�B	{�B	z�B	~�B	|�B	�B	�B	�B	�B	�%B	�1B	�7B	�=B	�JB	�\B	�bB	�bB	�bB	�bB	�hB	�oB	�{B	�{B	�{B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	��B	��B	�B	�B	�'B	�!B	�B	�B	�B	�B	�B	�!B	�'B	�-B	�3B	�9B	�9B	�?B	�FB	�FB	�LB	�LB	�XB	�dB	�wB	�}B	�}B	��B	��B	�}B	�wB	��B	ĜB	ƨB	ƨB	ȴB	ȴB	ȴB	ȴB	ȴB	ȴB	ȴB	ɺB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�
B	�#B	�)B	�)B	�/B	�/B	�/B	�/B	�/B	�5B	�5B	�5B	�NB	�TB	�TB	�TB	�ZB	�ZB	�TB	�TB	�TB	�NB	�TB	�NB	�HB	�BB	�BB	�HB	�TB	�ZB	�`B	�`B	�fB	�sB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B
+B
DB
uB
�B
!�B
$�B
)�B
/B
6FB
=qB
E�B
M�B
Q�B
XB
[#B
`BB
e`B
iyB
n�B
s�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111 B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B�B�B�0B�	B��B�B�NBÚBɽB��B��B��B�B�IB�GB�KB�iB�xB�B'B)�BVBw�B�	B�B�+B�'B�0B��B��B�XB�FB�GB�rB��B��B��B��B��B��B��B�B�oB��B�B�fBǰB�B�B��BØB��BȹB��B�RB��B��B�B��B�~B�BBy�Bl�B]4BQ�BA�B-B)�BlB	<B��B�PB��B��B��B�B�BǶB��B�Br�B^9BE�B!�B
��B
ŦB
��B
ffB
E�B
<mB
/B
'�B
�B

?B
MB	��B	�sB	�B	ȷB	�6B	��B	 B	p�B	_>B	VB	N�B	F�B	<mB	1)B	%�B	�B	�B	�B	�B	zB	JB	 B��B�B�~B�AB�4B�#B��BB�hB�MB�'B� B��B��B��B��B�B�\B�:B�By�Bx�Bw�Bt�Bs�Br�Bp�Bq�Bm�Bl�BiBhwBgpBfmBfjBgpBhyBj�Bi|BcWB_BB]3B[(B\-B^:BZ"B^9BcWBd`BbTBd_Bd`BedBd^BedBhwBl�Bp�Bp�Bs�Bt�Bs�Bu�Bu�Bu�Bu�Bx�B~�B�B�	B�B�
B�B�/B�PB�QB�bB�ZB�MB�DB� B�B}�B}�B|�Bz�Bx�Bx�By�B{�B|�B~�B�B�B�B�$B�7B�;B�>B�;B�CB�JB�fB�rB�kB�tB�zB�zB�lB�8B�B�B�2B��B��B��B�B�'B�,B��B�SB�>B�UB��B��B��BȺB��B�B�B�B�B��B	 B	OB	`B	lB	tB	{B	B	�B	�B	�B	�B	�B	�B	 �B	"�B	$�B	%�B	'�B	+B	+B	%�B	�B	 �B	#�B	$�B	'�B	,B	0%B	21B	7QB	:eB	E�B	J�B	J�B	F�B	G�B	H�B	H�B	K�B	Q�B	S�B	V
B	XB	XB	XB	XB	[(B	^:B	_BB	`IB	`IB	_@B	bUB	c[B	cWB	ecB	hyB	efB	flB	hwB	j�B	j�B	k�B	l�B	n�B	p�B	q�B	r�B	t�B	v�B	y�B	{�B	{�B	z�B	~�B	|�B	�B	�B	�B	�$B	�*B	�6B	�<B	�BB	�QB	�bB	�fB	�fB	�gB	�gB	�nB	�uB	��B	��B	�~B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�B	�B	�.B	�&B	�B	�B	�B	�B	�B	�&B	�.B	�1B	�7B	�>B	�=B	�DB	�JB	�LB	�SB	�QB	�]B	�iB	�|B	��B	��B	��B	��B	��B	�|B	��B	ģB	ƭB	ƭB	ȷB	ȻB	ȸB	ȺB	ȸB	ȸB	ȸB	ɿB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	��B	��B	��B	��B	��B	�B	�'B	�/B	�/B	�5B	�4B	�2B	�4B	�4B	�;B	�7B	�=B	�UB	�XB	�YB	�XB	�`B	�^B	�YB	�ZB	�YB	�TB	�YB	�RB	�MB	�FB	�FB	�KB	�ZB	�_B	�cB	�gB	�kB	�wB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��G�O�B	��B
.B
HB
{B
�B
!�B
$�B
*B
/!B
6KB
=uB
E�B
M�B
Q�B
XB
[*B
`FB
edB
iB
n�B
s�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111 <#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
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
PRES            TEMP            PSAL            PRES_ADJUSTED = PRES - dP                                                                                                                                                                                                                                       none                                                                                                                                                                                                                                                            PSAL_ADJUSTED = sw_salt( sw_cndr(PSAL,TEMP,PRES), TEMP, PRES_ADJUSTED )                                                                                                                                                                                         dP =0.04 dbar.                                                                                                                                                                                                                                                  none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            Pressures adjusted by using pressure offset at the sea surface. The quoted error is manufacturer specified accuracy in dbar.                                                                                                                                    The quoted error is manufacturer specified accuracy with respect to ITS-90 at time of laboratory calibration.                                                                                                                                                   No significant salinity drift detected. The quoted error is max[0.01, 1xOWC uncertainty] in PSS-78.                                                                                                                                                             201904081332452019040813324520190408133245  AO  ARCAADJP                                                                    20181121125826    IP                G�O�G�O�G�O�                AO  ARGQQCPL                                                                    20181121125826  QCP$                G�O�G�O�G�O�5F03E           AO  ARGQQCPL                                                                    20181121125826  QCF$                G�O�G�O�G�O�0               UW  ARSQUWQC    WOD & nearby Argo as visual check                               20190408133245  IP                  G�O�G�O�G�O�                