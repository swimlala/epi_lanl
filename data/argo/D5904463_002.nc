CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       N2015-02-26T22:14:45Z AOML 3.0 creation; 2016-08-07T22:44:57Z UW 3.1 conversion     
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
_FillValue                    �4Argo profile    3.1 1.2 19500101000000  20150226221445  20160807154457  5904463 US ARGO PROJECT                                                 STEPHEN RISER                                                   PRES            TEMP            PSAL               A   AO  5288_9005_002                   2C  D   APEX                            6530                            072314                          846 @�����1   @��K�@@*�;dZ�cs\(�1   GPS     Primary sampling: mixed [deeper than nominal 985dbar: discrete; nominal 985dbar to surface: 2dbar-bin averaged]                                                                                                                                                    A   B   B   @�  @�  A   A   A@  A`  A�  A�  A�  A�  A�  A�  A�  A���B33B33B33B��B   B)��B0��B733B@  BH  BP  BX  B`  Bh  Bp  Bx  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B���B���B�  B�  B�  B�  B�  B�  B�  B�33B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  C �C�C  C  C  C
  C  C  C  C  C  C  C  C�fC  C  C   C"  C$  C&  C(  C*  C,  C.�C0�C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D!��D"� D#  D#� D#��D$� D%  D%� D&  D&� D'  D'� D(fD(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DEfDE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Doy�Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dt� Dt��Dyl�D�3D�P D�y�D��fD� D�0 D�l�D�� D�fD�@ D�l�D��fD���D�33Dډ�D��3D��D�@ D�3D��f111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  @���@���AffA&ffAFffAfffA�33A�33A�33A�33A�33A�33A�33A�  B��B��B��B34B!��B+34B2fgB8��BA��BI��BQ��BY��Ba��Bi��Bq��By��B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B�  B���B���B���B���B���B���B���B���B���B���B���B���C � C� CffCffCffC
ffCffCffCffCffCffCffCffCL�CffCffC ffC"ffC$ffC&ffC(ffC*ffC,ffC.� C0� C2ffC4ffC6ffC8ffC:ffC<ffC>ffC@ffCBffCDffCFffCHffCJffCLffCNffCPffCRffCTffCVffCXffCZffC\ffC^ffC`ffCbffCdffCfffChffCjffClffCnffCpffCrffCtffCvffCxffCzffC|ffC~ffC�33C�33C�33C�33C�33C�33C�33C�33C�33C�33C�33C�33C�33C�33C�33C�33C�33C�33C�33C�33C�33C�33C�33C�33C�33C�33C�33C�33C�33C�33C�33C�33C�33C�33C�33C�33C�33C�33C�33C�33C�33C�33C�33C�33C�33C�33C�33C�33C�33C�33C�33C�33C�33C�33C�33C�33C�33C�33C�33C�33C�33C�33C�33C�33C�33C�33C�33C�33C�33C�33C�33C�33C�33C�33C�33C�33C�33C�33C�33C�33C�33C�33C�33C�33C�33C�33C�33C�33C�33C�33C�33C�33C�33C�33C�33C�33C�33C�33C�33C�33C�33C�33C�33C�33C�33C�33C�33C�33C�33C�@ C�33C�33C�33C�33C�&fC�33C�33C�33C�33C�33C�33C�33C�33C�33C�33C�33C�33C�33D �D ��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D	�D	��D
�D
��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D �D ��D!�D!��D"4D"��D#�D#��D$4D$��D%�D%��D&�D&��D'�D'��D(  D(��D)�D)��D*�D*��D+�D+��D,�D,��D-�D-��D.�D.��D/�D/��D0�D0��D1�D1��D2�D2��D3�D3��D4�D4��D5�D5��D6�D6��D7�D7��D8�D8��D9�D9��D:�D:��D;�D;��D<�D<��D=�D=��D>�D>��D?�D?��D@�D@��DA�DA��DB�DB��DC�DC��DD�DD��DE  DE��DF�DF��DG�DG��DH�DH��DI�DI��DJ�DJ��DK�DK��DL�DL��DM�DM��DN�DN��DO�DO��DP�DP��DQ�DQ��DR�DR��DS�DS��DT�DT��DU�DU��DV�DV��DW�DW��DX�DX��DY�DY��DZ�DZ��D[�D[��D\�D\��D]�D]��D^�D^��D_�D_��D`�D`��Da�Da��Db�Db��Dc�Dc��Dd�Dd��De�De��Df�Df��Dg�Dg��Dh�Dh��Di�Di��Dj�Dj��Dk�Dk��Dl�Dl��Dm�Dm��Dn�Dn��Do�Do�4Dp�Dp��Dq�Dq��Dr�Dr��Ds�Ds��Dt�Dt��Dt�gDy�gD�  D�\�D��gD��3D��D�<�D�y�D���D�3D�L�D�y�D��3D�	�D�@ DږgD�� D��D�L�D� D��3111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  @��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��A���A���A�ȴA�ȴA���A���A���A���A�ȴA�ƨA�ƨA�ĜA߼jA�33A��A܋DA�C�A�%Aɩ�A�r�Ař�A�%A�~�A��7A�M�A���A��A��A�E�A��A�r�A�ĜA��A�=qA��`A���A��A��mA���A�7LA�x�A�/A�n�A���A�E�A��RA�x�A�-A��FA��A��A���A���A��TA�33A���A�ĜA�/A��A��9AzbAk��Acx�A_��A\A�AY��AX��AU�FAQ��AN�AL  AJ��AF�A>��A;��A;VA9�-A9p�A9�A8�A8-A7��A7��A7�A7�A7\)A7�A7?}A8ZA7�A7oA5�A4{A2$�A0�/A0v�A0VA0�HA3�A0�jA/��A.��A.ZA.$�A.{A.  A-��A-A-`BA,�`A,bNA+��A+l�A*��A*��A*�RA*�jA)��A)A(^5A'��A'`BA&��A&��A'%A&�A&A�A%�A%
=A%S�A$1A!�A!;dA �`A  �A%A��A�RA�9A�A��Az�A(�Av�A��AĜA�A|�A/AoA
=AȴA�AM�A��A�A�wA��A��A�A�mAC�A�AdZAȴA�mAt�A�AĜAQ�A��A|�AXA7LA�A��AI�A=qAQ�A��AoA33A\)Ap�A;dA��Az�A5?A  A�TA��Ap�A��A�yA�!A�AO�A�A��A�A
�A
�uA	�A	��A	dZA	C�A	
=AȴA5?A�PA�HA�DAVA(�A��A�TAA�PA33A�A��AQ�A�A�AJA��A��A�7A?}A%A��A��AAXAoA �jA ^5A �@�S�@�n�@���@�G�@��@�|�@���@�~�@�G�@��@���@�Q�@�b@�K�@�J@��-@�x�@���@��@�@�+@�ȴ@�M�@���@�  @�C�@�!@���@�G�@�V@�A�@�S�@�M�@���@�9@�(�@�"�@���@��@�r�@�A�@��@�S�@�R@�ff@���@ᙚ@�hs@� �@�\)@ާ�@�5?@���@�hs@�/@�z�@۶F@�K�@�ȴ@�$�@ٙ�@�O�@��`@�j@�1@�l�@��H@���@֗�@֗�@֗�@֗�@�n�@��@ԛ�@�Q�@� �@��@�|�@�ȴ@�v�@�@ёh@�X@���@�bN@��
@ϕ�@�@���@·+@��T@͑h@��@̃@�  @˕�@�dZ@�^5@���@�G�@ț�@�I�@���@ǶF@�+@�~�@Ɨ�@Ɨ�@�@š�@�7L@�V@ļj@�r�@�  @öF@Å@���@�v�@�@��@���@��@�Q�@�1@�;d@�M�@���@�p�@�O�@�/@���@��j@�(�@��;@�|�@�
=@���@�n�@��^@�x�@�&�@��u@�A�@��
@�t�@��@�v�@��T@�X@�%@��`@���@�Ĝ@��9@��u@�r�@�bN@�Z@�  @��@�C�@�"�@�V@�E�@�5?@���@��^@�x�@�?}@��/@��j@���@�(�@���@�;d@��@��!@��+@�ff@�{@���@��9@�I�@�9X@�(�@��@��@�K�@�ȴ@��\@�=q@��#@��-@�X@���@��`@��D@�b@���@�\)@�o@��!@��+@�V@�5?@�J@��@�`B@���@�9X@��@��
@��P@�K�@��@��R@�~�@�{@��T@��h@���@�bN@���@��@�33@��y@�ff@��T@���@�V@���@��@�(�@��w@�\)@���@���@���@��+@�V@�{@���@�?}@��@���@�z�@�9X@��@��F@�?}@�t�@��/@��\@xr�@m/@d��@]p�@TZ@Ko@B��@<j@7+@1��@-�-@'|�@"�@�@�P@�`@V111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111  A���A���A�ȴA�ȴA���A���A���A���A�ȴA�ƨA�ƨA�ĜA߼jA�33A��A܋DA�C�A�%Aɩ�A�r�Ař�A�%A�~�A��7A�M�A���A��A��A�E�A��A�r�A�ĜA��A�=qA��`A���A��A��mA���A�7LA�x�A�/A�n�A���A�E�A��RA�x�A�-A��FA��A��A���A���A��TA�33A���A�ĜA�/A��A��9AzbAk��Acx�A_��A\A�AY��AX��AU�FAQ��AN�AL  AJ��AF�A>��A;��A;VA9�-A9p�A9�A8�A8-A7��A7��A7�A7�A7\)A7�A7?}A8ZA7�A7oA5�A4{A2$�A0�/A0v�A0VA0�HA3�A0�jA/��A.��A.ZA.$�A.{A.  A-��A-A-`BA,�`A,bNA+��A+l�A*��A*��A*�RA*�jA)��A)A(^5A'��A'`BA&��A&��A'%A&�A&A�A%�A%
=A%S�A$1A!�A!;dA �`A  �A%A��A�RA�9A�A��Az�A(�Av�A��AĜA�A|�A/AoA
=AȴA�AM�A��A�A�wA��A��A�A�mAC�A�AdZAȴA�mAt�A�AĜAQ�A��A|�AXA7LA�A��AI�A=qAQ�A��AoA33A\)Ap�A;dA��Az�A5?A  A�TA��Ap�A��A�yA�!A�AO�A�A��A�A
�A
�uA	�A	��A	dZA	C�A	
=AȴA5?A�PA�HA�DAVA(�A��A�TAA�PA33A�A��AQ�A�A�AJA��A��A�7A?}A%A��A��AAXAoA �jA ^5A �@�S�@�n�@���@�G�@��@�|�@���@�~�@�G�@��@���@�Q�@�b@�K�@�J@��-@�x�@���@��@�@�+@�ȴ@�M�@���@�  @�C�@�!@���@�G�@�V@�A�@�S�@�M�@���@�9@�(�@�"�@���@��@�r�@�A�@��@�S�@�R@�ff@���@ᙚ@�hs@� �@�\)@ާ�@�5?@���@�hs@�/@�z�@۶F@�K�@�ȴ@�$�@ٙ�@�O�@��`@�j@�1@�l�@��H@���@֗�@֗�@֗�@֗�@�n�@��@ԛ�@�Q�@� �@��@�|�@�ȴ@�v�@�@ёh@�X@���@�bN@��
@ϕ�@�@���@·+@��T@͑h@��@̃@�  @˕�@�dZ@�^5@���@�G�@ț�@�I�@���@ǶF@�+@�~�@Ɨ�@Ɨ�@�@š�@�7L@�V@ļj@�r�@�  @öF@Å@���@�v�@�@��@���@��@�Q�@�1@�;d@�M�@���@�p�@�O�@�/@���@��j@�(�@��;@�|�@�
=@���@�n�@��^@�x�@�&�@��u@�A�@��
@�t�@��@�v�@��T@�X@�%@��`@���@�Ĝ@��9@��u@�r�@�bN@�Z@�  @��@�C�@�"�@�V@�E�@�5?@���@��^@�x�@�?}@��/@��j@���@�(�@���@�;d@��@��!@��+@�ff@�{@���@��9@�I�@�9X@�(�@��@��@�K�@�ȴ@��\@�=q@��#@��-@�X@���@��`@��D@�b@���@�\)@�o@��!@��+@�V@�5?@�J@��@�`B@���@�9X@��@��
@��P@�K�@��@��R@�~�@�{@��T@��h@���@�bN@���@��@�33@��y@�ff@��T@���@�V@���@��@�(�@��w@�\)@���@���@���@��+@�V@�{@���@�?}@��@���@�z�@�9X@��G�O�@�?}@�t�@��/@��\@xr�@m/@d��@]p�@TZ@Ko@B��@<j@7+@1��@-�-@'|�@"�@�@�P@�`@V111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111  ;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oG�O�;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oB
s�B
s�B
r�B
r�B
s�B
s�B
s�B
s�B
s�B
s�B
s�B
s�B
s�B
y�B
�JB
��B
��B��B�wB�B�BPB�B�B<jBgmBhsB�+B�%B�B�%B�DB�DB�\B��B��B��B��B�\Bm�BH�B:^BE�B;dB33B�B��B�}B�oB� B�1BF�B
��B
�ZB
�jB
��B
��B
q�B
>wB
PB	��B	F�B	1'B	/B	0!B	<jB	?}B	B�B	D�B	T�B	hsB	ffB	B�B	bB	+B	DB	 �B	#�B	&�B	-B	6FB	P�B	n�B	y�B	�DB	��B	�B	�}B
B
�B
�B
�B
�B
�B
bB
\B
hB
�B
H�B
:^B
/B
'�B
'�B
&�B
&�B
'�B
&�B
)�B
(�B
'�B
.B
33B
6FB
>wB
L�B
M�B
XB
T�B
Q�B
R�B
XB
ZB
\)B
aHB
iyB
m�B
gmB
aHB
aHB
e`B
^5B
H�B
N�B
N�B
I�B
H�B
N�B
O�B
P�B
T�B
YB
ZB
XB
N�B
G�B
C�B
D�B
B�B
B�B
B�B
B�B
B�B
C�B
F�B
I�B
J�B
J�B
P�B
VB
XB
^5B
\)B
W
B
P�B
L�B
K�B
K�B
K�B
L�B
M�B
Q�B
Q�B
Q�B
Q�B
Q�B
Q�B
VB
XB
aHB
l�B
r�B
u�B
x�B
z�B
{�B
z�B
x�B
w�B
v�B
u�B
t�B
q�B
p�B
q�B
p�B
m�B
iyB
ffB
aHB
^5B
\)B
ZB
XB
XB
W
B
VB
T�B
R�B
P�B
M�B
J�B
I�B
I�B
H�B
H�B
G�B
F�B
F�B
H�B
H�B
G�B
G�B
G�B
G�B
I�B
K�B
K�B
K�B
J�B
H�B
G�B
D�B
B�B
A�B
?}B
>wB
=qB
<jB
;dB
:^B
<jB
;dB
9XB
7LB
6FB
5?B
5?B
6FB
5?B
5?B
6FB
5?B
49B
33B
2-B
2-B
2-B
2-B
1'B
0!B
/B
-B
,B
+B
)�B
(�B
(�B
)�B
(�B
'�B
&�B
$�B
%�B
$�B
"�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
{B
{B
�B
�B
�B
�B
�B
�B
�B
�B
�B
{B
{B
�B
�B
�B
�B
�B
�B
�B
�B
{B
uB
{B
{B
{B
{B
uB
uB
oB
oB
hB
hB
bB
PB
JB
JB
JB
PB
JB
\B
VB
JB
VB
bB
\B
hB
bB
bB
bB
\B
bB
bB
bB
hB
hB
bB
\B
VB
VB
\B
\B
\B
VB
VB
VB
VB
VB
VB
PB
PB
PB
PB
PB
PB
JB
PB
PB
PB
VB
VB
VB
VB
VB
\B
\B
bB
bB
bB
bB
bB
bB
bB
bB
bB
bB
bB
bB
bB
bB
bB
bB
bB
bB
bB
bB
bB
hB
uB
uB
uB
uB
uB
uB
uB
uB
uB
uB
uB
uB
uB
uB
uB
oB
oB
oB
uB
oB
uB
uB
uB
{B
{B
{B
{B
{B
{B
{B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
&�B
!�B
'�B
/B
5?B
=qB
D�B
H�B
M�B
R�B
XB
]/B
bNB
e`B
iyB
k�B
m�B
r�B
u�B
y�B
� B
�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111  B
s�B
s�B
r�B
r�B
s�B
s�B
s�B
s�B
s�B
s�B
s�B
s�B
s�B
y�B
�/B
��B
��B��B�YB�wB�B+B�B�B<JBgLBhNB�	B�B��B�B�"B�"B�7B�cB�iB�aB�`B�7BmnBH�B:8BE{B;AB3
B`B��B�WB�KB�B�BF�B
��B
�6B
�GB
��B
�jB
q�B
>UB
,B	��B	F�B	1	B	/B	0B	<MB	?aB	BpB	D�B	T�B	hUB	fGB	BpB	CB	B	&B	 �B	#�B	&�B	,�B	6&B	P�B	nwB	y�B	�"B	��B	��B	�[B
�B
hB
�B
�B
�B
mB
=B
5B
AB
�B
H�B
:3B
.�B
'�B
'�B
&�B
&�B
'�B
&�B
)�B
(�B
'�B
-�B
3
B
6B
>MB
L�B
M�B
W�B
T�B
Q�B
R�B
W�B
Y�B
\B
aB
iPB
mgB
gDB
aB
aB
e7B
^B
H�B
N�B
N�B
I�B
H�B
N�B
O�B
P�B
T�B
X�B
Y�B
W�B
N�B
G�B
CkB
DsB
BdB
BdB
BcB
BgB
BdB
CmB
F~B
I�B
J�B
J�B
P�B
U�B
W�B
^B
[�B
V�B
P�B
L�B
K�B
K�B
K�B
L�B
M�B
Q�B
Q�B
Q�B
Q�B
Q�B
Q�B
U�B
W�B
aB
l`B
r�B
u�B
x�B
z�B
{�B
z�B
x�B
w�B
v�B
u�B
t�B
qB
puB
q|B
pvB
mdB
iKB
f:B
aB
^
B
[�B
Y�B
W�B
W�B
V�B
U�B
T�B
R�B
P�B
M�B
J�B
I�B
I�B
H�B
H�B
G�B
F|B
F{B
H�B
H�B
G�B
G�B
G�B
G�B
I�B
K�B
K�B
K�B
J�B
H�B
G�B
DmB
BdB
A]B
?RB
>LB
=DB
<>B
;9B
:1B
<>B
;:B
9+B
7B
6B
5B
5B
6B
5B
5B
6B
5B
4B
3	B
2B
2B
2B
2B
0�B
/�B
.�B
,�B
+�B
*�B
)�B
(�B
(�B
)�B
(�B
'�B
&�B
$�B
%�B
$�B
"�B
�B
�B
�B
|B
tB
rB
qB
rB
nB
oB
iB
kB
cB
aB
iB
iB
gB
`B
cB
aB
cB
ZB
ZB
\B
UB
VB
VB
OB
NB
TB
TB
UB
UB
UB
UB
UB
RB
SB
MB
NB
SB
UB
TB
SB
UB
]B
[B
SB
NB
IB
MB
OB
NB
MB
IB
IB
BB
BB
:B
<B
5B
$B
B
B
B
$B
B
0B
+B
B
+B
5B
2B
;B
6B
5B
6B
0B
6B
6B
3B
;B
<B
6B
0B
,B
(B
0B
.B
/B
*B
,B
)B
)B
*B
+B
#B
#B
#B
#B
"B
$B
B
 B
!B
"B
)B
*B
)B
)B
)B
/B
/B
2B
6B
6B
4B
5B
7B
7B
6B
6B
6B
5B
4B
5B
5B
6B
7B
6B
6B
6B
6B
8B
;B
FB
GB
IB
HB
JB
GB
JB
JB
IB
GB
GB
HB
HB
HB
HB
BB
BB
@B
FB
AB
HB
GB
IB
NB
NB
NB
OB
OB
NB
KB
TB
XB
ZB
eB
gB
lB
eB
gB
gB
lB
mB
lB
lB
pB
rB
pB
sB
kB
kB
mB
oB
kB
lB
lB
lB
mB
pB
rB
rB
yB
yB
zB
wB
~B
~B
~B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�G�O�B
!�B
'�B
.�B
5B
=AB
DmB
H�B
M�B
R�B
W�B
\�B
b B
e0B
iJB
kVB
maB
r�B
u�B
y�B
�B
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
PRES            TEMP            PSAL            PRES_ADJUSTED = PRES - dP                                                                                                                                                                                                                                       none                                                                                                                                                                                                                                                            PSAL_ADJUSTED = sw_salt( sw_cndr(PSAL,TEMP,PRES), TEMP, PRES_ADJUSTED )                                                                                                                                                                                         dP =-0.4 dbar.                                                                                                                                                                                                                                                  none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            Pressures adjusted by using pressure offset at the sea surface. The quoted error is manufacturer specified accuracy in dbar.                                                                                                                                    The quoted error is manufacturer specified accuracy with respect to ITS-90 at time of laboratory calibration.                                                                                                                                                   No significant salinity drift detected. Salinity adjusted for effects of pressure adjustment. The quoted error is max[0.01, 1xOW uncertainty] in PSS-78.                                                                                                        201608071544572016080715445720160807154457  AO  ARCAADJP                                                                    20150226221445    IP                G�O�G�O�G�O�                AO  ARGQQCPL                                                                    20150226221445  QCP$                G�O�G�O�G�O�DFB5E           AO  ARGQQCPL                                                                    20150226221445  QCF$                G�O�G�O�G�O�0               UW  ARSQUWQC    WOD & nearby Argo as visual check                               20160807154457  IP                  G�O�G�O�G�O�                