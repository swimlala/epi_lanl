CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             
   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       2020-06-18T14:14:01Z creation      
references        (http://www.argodatamgt.org/Documentation   comment       	free text      user_manual_version       3.2    Conventions       Argo-3.2 CF-1.6    featureType       trajectoryProfile      comment_dmqc_operator         ZPRIMARY | https://orcid.org/0000-0001-7324-3159 | Matthew Alkire, University of Washington        @   	DATA_TYPE                  	long_name         	Data type      conventions       Argo reference table 1     
_FillValue                    7   FORMAT_VERSION                 	long_name         File format version    
_FillValue                    7(   HANDBOOK_VERSION               	long_name         Data handbook version      
_FillValue                    7,   REFERENCE_DATE_TIME                 	long_name         !Date of reference for Julian days      conventions       YYYYMMDDHHMISS     
_FillValue                    70   DATE_CREATION                   	long_name         Date of file creation      conventions       YYYYMMDDHHMISS     
_FillValue                    7@   DATE_UPDATE                 	long_name         Date of update of this file    conventions       YYYYMMDDHHMISS     
_FillValue                    7P   PLATFORM_NUMBER                   	long_name         Float unique identifier    conventions       WMO float identifier : A9IIIII     
_FillValue                    7`   PROJECT_NAME                  	long_name         Name of the project    
_FillValue                  @  7h   PI_NAME                   	long_name         "Name of the principal investigator     
_FillValue                  @  7�   STATION_PARAMETERS           	            	long_name         ,List of available parameters for the station   conventions       Argo reference table 3     
_FillValue                  0  7�   CYCLE_NUMBER               	long_name         Float cycle number     conventions       =0...N, 0 : launch cycle (if exists), 1 : first complete cycle      
_FillValue         ��        8   	DIRECTION                  	long_name         !Direction of the station profiles      conventions       -A: ascending profiles, D: descending profiles      
_FillValue                    8   DATA_CENTRE                   	long_name         .Data centre in charge of float data processing     conventions       Argo reference table 4     
_FillValue                    8    DC_REFERENCE                  	long_name         (Station unique identifier in data centre   conventions       Data centre convention     
_FillValue                     8$   DATA_STATE_INDICATOR                  	long_name         1Degree of processing the data have passed through      conventions       Argo reference table 6     
_FillValue                    8D   	DATA_MODE                  	long_name         Delayed mode or real time data     conventions       >R : real time; D : delayed mode; A : real time with adjustment     
_FillValue                    8H   PLATFORM_TYPE                     	long_name         Type of float      conventions       Argo reference table 23    
_FillValue                     8L   FLOAT_SERIAL_NO                   	long_name         Serial number of the float     
_FillValue                     8l   FIRMWARE_VERSION                  	long_name         Instrument firmware version    
_FillValue                     8�   WMO_INST_TYPE                     	long_name         Coded instrument type      conventions       Argo reference table 8     
_FillValue                    8�   JULD               	long_name         ?Julian day (UTC) of the station relative to REFERENCE_DATE_TIME    standard_name         time   units         "days since 1950-01-01 00:00:00 UTC     conventions       8Relative julian days with decimal part (as parts of day)   
resolution        >�EȠ      
_FillValue        A.�~       axis      T           8�   JULD_QC                	long_name         Quality on date and time   conventions       Argo reference table 2     
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
_FillValue                    �   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                    �   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                    �   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                    �   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  @  �   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                    �T   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                    �d   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    �h   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar         �x   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar         �|   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�        ��   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    ��Argo profile    3.1 1.2 19500101000000  20200618141401  20220204114420  5905979 UW, Argo                                                        STEPHEN RISER                                                   PRES            TEMP            PSAL               [A   AO  7662                            2C  D   APEX                            8312                            080318                          846 @���r�:1   @��ޕ��l@6�bM���c�ȴ9X1   GPS     Primary sampling: mixed [deeper than nominal 985dbar: discrete; nominal 985dbar to surface: 2dbar-bin averaged]                                                                                                                                                    [A   B   B   @���@�  A   A   A@  A`  A�  A�  A�  A�  A�  A�  A�  A�  B   B  B  B  B   B(  B0  B8  B@  BH  BPffBX  B`  Bg��Bo��Bx  B�33B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�33B�33B�  B���B�  B�  B�  B�  B�  B�  B�  B�  B���B�  B�  B�  B�  B�  B�  B�  B�  C   C  C  C  C  C
  C  C  C  C  C  C  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CW�fCZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2�fD3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9y�D9��D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DDy�DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DO��DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DWy�DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_�fD`  D`� Da  Da� Db  Dby�Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dt� Dt�3Dy�qD��D�O
D���D���D�{D�S3D���D���D�qD�\)D��RD��
D��D�S3Dڋ3D��\D�%qD�QHD��D��)111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  @�33@�ff@�ffA33A;33A[33A{33A���A���A���A���A͙�Aݙ�A홚A���B��B��B��B��B&��B.��B6��B>��BF��BO33BV��B^��BffgBnfgBv��B33B�ffB�ffB�ffB�ffB�ffB�ffB�ffB�ffB�ffB�ffB���B���B�ffB�33B�ffB�ffB�ffB�ffB�ffB�ffB�ffB�ffB�33B�ffB�ffB�ffB�ffB�ffB�ffB�ffB�ffB�ffC�3C�3C�3C�3C	�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C!�3C#�3C%�3C'�3C)�3C+�3C-�3C/�3C1�3C3�3C5�3C7�3C9�3C;�3C=�3C?�3CA�3CC�3CE�3CG�3CI�3CK�3CM�3CO�3CQ�3CS�3CU�3CW��CY�3C[�3C]�3C_�3Ca�3Cc�3Ce�3Cg�3Ci�3Ck�3Cm�3Co�3Cq�3Cs�3Cu�3Cw�3Cy�3C{�3C}�3C�3C�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC���C�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC���C�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚD l�D ��Dl�D��Dl�D��Dl�D��Dl�D��Dl�D��Dl�D��Dl�D��Dl�D��D	l�D	��D
l�D
��Dl�D��Dl�D��Dl�D��Dl�D��Dl�D��Dl�D��Dl�D��Dl�D��Dl�D��Dl�D��Dl�D��Dl�D��Dl�D��Dl�D��Dl�D��Dl�D��Dl�D��Dl�D��Dl�D��Dl�D��Dl�D��D l�D ��D!l�D!��D"l�D"��D#l�D#��D$l�D$��D%l�D%��D&l�D&��D'l�D'��D(l�D(��D)l�D)��D*l�D*��D+l�D+��D,l�D,��D-l�D-��D.l�D.��D/l�D/��D0l�D0��D1l�D1��D2s3D2��D3l�D3��D4l�D4��D5l�D5��D6l�D6��D7l�D7��D8l�D8��D9fgD9�gD:l�D:��D;l�D;��D<l�D<��D=l�D=��D>l�D>��D?l�D?��D@l�D@��DAl�DA��DBl�DB��DCl�DC��DDfgDD��DEl�DE��DFl�DF��DGl�DG��DHl�DH��DIl�DI��DJl�DJ��DKl�DK��DLl�DL��DMl�DM��DNl�DN��DOl�DO�gDPl�DP��DQl�DQ��DRl�DR��DSl�DS��DTl�DT��DUl�DU��DVl�DV��DWfgDW��DXl�DX��DYl�DY��DZl�DZ��D[l�D[��D\l�D\��D]l�D]��D^l�D^��D_s3D_��D`l�D`��Dal�Da��DbfgDb��Dcl�Dc��Ddl�Dd��Del�De��Dfl�Df��Dgl�Dg��Dhl�Dh��Dil�Di��Djl�Dj��Dkl�Dk��Dll�Dl��Dml�Dm��Dnl�Dn��Dol�Do��Dpl�Dp��Dql�Dq��Drl�Dr��Dsl�Ds��Dtl�Dt� Dy�>D�D�EpD��)D��3D�
�D�I�D��)D��\D��D�R�D���D��pD�GD�I�Dځ�D���D��D�G�D�GD���111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  @��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��A־wA־wAּjAָRA־wAֺ^AּjAֺ^Aֺ^AּjA־wAּjA���A�ȴA�ƨA�ȴA�ȴA�ȴA�ĜA�ȴA���A�ȴA�ƨA֧�A�E�AՑhA�JA�?}A�l�A�ĜA�JA�1A���A�9XAŁA��A��;A�z�A���A�ffA�{A�ƨA�E�A��A�t�A��yA�=qA�A��DA���A���A��A��
A�%A�\)A�33A���A�ZA�1'A�{A�VA�VA��uA��A��A�jA�1A�~�A�v�A��+A��jA�ȴA�O�A� �A�^5A�\)A��A�+A�/A��!A�~�A���A�p�A��A�v�A�n�A���A�9XA�{A�A���A��9A�G�A�ffA�S�A�E�A��A��PA��`A�XA�x�A���A�(�A�~�A��wA���A��A���A���A��A�v�A���A���A�1'A��A��`A��A�bA~�jA{+Ay��AxAu7LAs�mAr�Ao��Ak��Ai"�Ah  Af�`Ag%Agx�Af�Ac��A`$�A^bA]K�A\�!A\z�A[p�AY��AX��AW�PAV�/AVI�AU�hARĜAQ;dAOhsAMdZAK�FAKAJ�!AJ �AG�AF�RAE�TAE�ADJAB��A@�A?XA>��A=S�A< �A;��A;�A9�;A8��A8r�A7�
A5�;A3ƨA1�wA0�A0ZA0ZA09XA.�9A,E�A+��A+&�A*��A*bA)p�A(��A(JA'G�A%�#A%l�A$�A#�PA"�A ��A n�A bA�AC�AJA��AȴA�wA�HAA�AS�A�A1'A��A��A1AĜA�AXAȴAffA9XA��A�HAI�A��A	ƨA�A�7A  AdZA33A/A/A+A�HA��A��A�A ĜA 1@�+@��+@��@��@�ȴ@�5?@�@�`B@�  @���@�G�@��@�@�"�@���@�E�@�G�@��@���@�o@��@�K�@�=q@��`@�"�@��@�Ĝ@�l�@�ȴ@ٙ�@���@��@֗�@� �@��T@�7L@Ь@�"�@θR@�-@�x�@�V@̣�@���@�ȴ@��@�G�@�Ĝ@�dZ@�hs@���@�|�@��@�@�p�@���@�|�@��R@�E�@���@��@��j@�;d@�v�@���@��m@���@���@�J@�`B@��@��;@�dZ@�-@���@�x�@�O�@�?}@�?}@��/@�1'@��H@�@���@�O�@��u@���@��;@���@��P@�\)@���@�ȴ@��@���@���@��\@�5?@��/@�1'@��@���@�
=@�$�@���@��@�(�@��@�Q�@�(�@�+@��H@�=q@���@���@��@���@��@���@���@�I�@�r�@��u@���@�v�@���@��H@�ȴ@���@��+@�{@�Q�@���@�@�?}@�hs@�@�V@�p�@�  @�C�@��@�\)@�-@�n�@���@�A�@���@�@�@���@�p�@�X@�/@��@�Q�@�1'@�(�@��@��F@�t�@�S�@�33@�o@��@��@��P@���@���@��\@��@�1@�j@�z�@�%@�dZ@�O�@�Ĝ@��u@��9@���@��/@�%@�G�@���@�X@��@�r�@�r�@�Z@�1'@�  @���@���@�t�@�K�@��@��H@��@���@�5?@��@�@�/@�Ĝ@��@�I�@���@�l�@�S�@�;d@�+@�
=@���@�=q@�@�X@�?}@�&�@�&�@��/@�r�@�Z@��@��m@��F@�dZ@�ȴ@���@��R@���@�M�@��@��T@���@��-@���@��7@�X@��@���@���@��9@��@���@��@�Z@� �@���@���@��@�\)@�K�@���@��@~��@u�@ihs@bC�@Z��@T��@I�^@Aԕ@<@7qv@1�D@,�j@%�@"��@�@q@W?@(�@
��111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111141111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111  A־wA־wAּjAָRA־wAֺ^AּjAֺ^Aֺ^AּjA־wAּjA���A�ȴA�ƨA�ȴA�ȴA�ȴA�ĜA�ȴA���A�ȴA�ƨA֧�A�E�AՑhA�JA�?}A�l�A�ĜA�JA�1A���A�9XAŁA��A��;A�z�A���A�ffA�{A�ƨA�E�A��A�t�A��yA�=qA�A��DA���A���A��A��
A�%A�\)A�33A���A�ZA�1'A�{A�VA�VA��uA��A��A�jA�1A�~�A�v�A��+A��jA�ȴA�O�A� �A�^5A�\)A��A�+A�/A��!A�~�A���A�p�A��A�v�A�n�A���A�9XA�{A�A���A��9A�G�A�ffA�S�A�E�A��A��PA��`A�XG�O�A���A�(�A�~�A��wA���A��A���A���A��A�v�A���A���A�1'A��A��`A��A�bA~�jA{+Ay��AxAu7LAs�mAr�Ao��Ak��Ai"�Ah  Af�`Ag%Agx�Af�Ac��A`$�A^bA]K�A\�!A\z�A[p�AY��AX��AW�PAV�/AVI�AU�hARĜAQ;dAOhsAMdZAK�FAKAJ�!AJ �AG�AF�RAE�TAE�ADJAB��A@�A?XA>��A=S�A< �A;��A;�A9�;A8��A8r�A7�
A5�;A3ƨA1�wA0�A0ZA0ZA09XA.�9A,E�A+��A+&�A*��A*bA)p�A(��A(JA'G�A%�#A%l�A$�A#�PA"�A ��A n�A bA�AC�AJA��AȴA�wA�HAA�AS�A�A1'A��A��A1AĜA�AXAȴAffA9XA��A�HAI�A��A	ƨA�A�7A  AdZA33A/A/A+A�HA��A��A�A ĜA 1@�+@��+@��@��@�ȴ@�5?@�@�`B@�  @���@�G�@��@�@�"�@���@�E�@�G�@��@���@�o@��@�K�@�=q@��`@�"�@��@�Ĝ@�l�@�ȴ@ٙ�@���@��@֗�@� �@��T@�7L@Ь@�"�@θR@�-@�x�@�V@̣�@���@�ȴ@��@�G�@�Ĝ@�dZ@�hs@���@�|�@��@�@�p�@���@�|�@��R@�E�@���@��@��j@�;d@�v�@���@��m@���@���@�J@�`B@��@��;@�dZ@�-@���@�x�@�O�@�?}@�?}@��/@�1'@��H@�@���@�O�@��u@���@��;@���@��P@�\)@���@�ȴ@��@���@���@��\@�5?@��/@�1'@��@���@�
=@�$�@���@��@�(�@��@�Q�@�(�@�+@��H@�=q@���@���@��@���@��@���@���@�I�@�r�@��u@���@�v�@���@��H@�ȴ@���@��+@�{@�Q�@���@�@�?}@�hs@�@�V@�p�@�  @�C�@��@�\)@�-@�n�@���@�A�@���@�@�@���@�p�@�X@�/@��@�Q�@�1'@�(�@��@��F@�t�@�S�@�33@�o@��@��@��P@���@���@��\@��@�1@�j@�z�@�%@�dZ@�O�@�Ĝ@��u@��9@���@��/@�%@�G�@���@�X@��@�r�@�r�@�Z@�1'@�  @���@���@�t�@�K�@��@��H@��@���@�5?@��@�@�/@�Ĝ@��@�I�@���@�l�@�S�@�;d@�+@�
=@���@�=q@�@�X@�?}@�&�@�&�@��/@�r�@�Z@��@��m@��F@�dZ@�ȴ@���@��R@���@�M�@��@��T@���@��-@���@��7@�X@��@���@���@��9@��@���@��@�Z@� �@���@���@��@�\)G�O�@���@��@~��@u�@ihs@bC�@Z��@T��@I�^@Aԕ@<@7qv@1�D@,�j@%�@"��@�@q@W?@(�@
��111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111141111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111  ;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oG�O�;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oG�O�;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�
B	�#B	�/B	�/B	�yB
B
�B
=qB
XB
aHB
iyB
p�B
�B
�bB
��B
�B
�3B
�^B
ŢB
��B
�TBVB0!B=qBS�BjB�DB��B�!B�FB�qB��B��B�/B��B��B+BDB�B)�B2-B33B2-B-B�BPB+B\BPB"�BVB�B�
B33BYBZBVBVBB�BH�BC�B?}BH�BH�BG�BG�BF�BA�B�B�BB�B��B�BF�B@�B�TBB�BT�BK�BH�B5?B�BJB�B%B
��B
�)B
�wB
��B
�B
Q�B
Q�B
?}B
�B
\B
B	�B	�/B	�B	�wB	��B	� B	v�B	r�B	�+B	��B	��B	�+B	hsB	\)B	ZB	YB	]/B	\)B	P�B	K�B	H�B	F�B	B�B	?}B	2-B	+B	�B	�B	hB	JB	DB	1B��B��B��B�B�B�sB�BB�B�
B��BŢB�wB�jB�RB�9B�!B�9B�B��B��B��B��B��B��B��B��B�uB�oB�bB�VB�PB�7B�1B�B�B~�B}�By�Bw�Bq�Bq�Bo�Bn�Bm�BjBhsBe`BaHB_;B]/BW
BVBT�BR�BP�BM�BJ�BH�BF�BF�BG�BF�BE�BE�BE�BI�BF�B=qB6FB7LB5?B7LB6FB6FB6FB5?B6FB7LB5?B7LB5?B6FB7LB6FB9XB;dB=qB<jB;dB=qB>wB>wB@�BB�BD�BE�BF�BF�BE�BD�BA�BD�BE�BE�BF�BF�BD�BE�BH�BF�BE�BD�BD�BI�BJ�BJ�BK�BJ�BL�BL�BO�BT�BZBYBYB_;B[#BXBVBVBXBYB[#B\)B\)B^5B`BBbNBffBgmBgmBhsBhsBhsBk�Bl�Bn�Bs�Bu�Bu�Bw�By�By�B}�B~�B�B�%B�+B�+B�1B�1B�PB�\B�oB��B��B��B��B��B�B�B�B�!B�FB�RB�XB�^B�^B�dB�qBĜBŢBǮBȴB��B��B�B��B�
B�)B�5B�HB�ZB�`B�sB�sB�sB�B�B�B��B��B��B	B	%B	DB	�B	 �B	#�B	%�B	)�B	-B	,B	(�B	#�B	"�B	%�B	)�B	/B	;dB	;dB	7LB	5?B	7LB	<jB	T�B	XB	W
B	S�B	VB	e`B	gmB	hsB	hsB	jB	m�B	u�B	y�B	y�B	y�B	y�B	{�B	|�B	}�B	~�B	~�B	� B	� B	z�B	v�B	u�B	v�B	y�B	� B	�B	�B	�1B	�1B	� B	|�B	|�B	� B	�B	�B	�B	�7B	�PB	�uB	��B	�{B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�!B	�!B	�!B	�!B	�!B	�'B	�-B	�3B	�3B	�3B	�RB	�^B	�jB	�}B	��B	B	ŢB	ƨB	ǮB	ǮB	ǮB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�
B	�B	�B	�B	�B	�NB	�B
�B
�B
\B
{B
 vB
.IB
3�B
:�B
@�B
D3B
K)B
QhB
WsB
[	B
_�B
dZB
jeB
p;B
r�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111141111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111  B	ɶB	ɶB	ɶB	ɶB	ɶB	ɶB	ɶB	ɶB	ɶB	ɶB	ɶB	ɶB	ɶB	ɶB	ʼB	ʼB	ʼB	ʼB	ʼB	ʼB	ʼB	ʼB	ʼB	��B	��B	��B	��B	��B	�CB	��B
IB
57B
O�B
YB
a=B
hhB
z�B
�$B
��B
��B
��B
�B
�bB
ȤB
�BB'�B5*BK�Bb5B��B��B��B��B�#B�rB̯B��B�xB��B��B�B_B!�B)�B*�B)�B$�BlB�B��BB BBB�8BξB*�BP�BQ�BM�BM�B:=B@bB;DB7,B@bB@bB?\B?\B>VB98BiB3B��B�MB�BpB>WB83G�O�B�HB|�BL�BC�B@vB-B}BBSB
��B
��B
��B
�DB
��B
x�B
I�B
I�B
7TB
zB
6B	��B	�hB	�B	��B	�XB	�~B	w�B	n�B	j�B	B	��B	��B	B	`[B	TB	RB	QB	UB	TB	H�B	C�B	@�B	>�B	:{B	7iB	*B	"�B	�B	�B		XB	;B	5B	 "B��B��B��B�B�B�gB�7B�B� B��B��B�oB�bB�JB�2B�B�2B��B��B��B��B��B��B��B��B��B�rB�lB�_B�SB�MB�5B�/B|B{Bv�Bu�Bq�Bo�Bi�Bi�Bg�Bf�Be�Bb�B`uB]bBYJBW>BU2BOBNBMBJ�BH�BE�BB�B@�B>�B>�B?�B>�B=�B=�B=�BA�B>�B5xB.NB/TB-GB/TB.OB.NB.NB-HB.OB/UB-HB/UB-HB.OB/UB.OB1aB3mB5zB4sB3mB5zB6�B6�B8�B:�B<�B=�B>�B>�B=�B<�B9�B<�B=�B=�B>�B>�B<�B=�B@�B>�B=�B<�B<�BA�BB�BB�BC�BB�BD�BD�BG�BM
BR(BQ"BQ"BWFBS/BPBNBNBPBQ#BS/BT5BT5BVABXNBZZB^rB_yB_yB`B`B`Bc�Bd�Bf�Bk�Bm�Bm�Bo�Bq�Bq�Bu�BwB}*B~0B6B6B�<B�<B�[B�gB�yB��B��B��B��B��B�B�$B�$B�*B�OB�[B�`B�fB�fB�lB�yB��B��B��B��B��B��B�B�B�B�0B�<B�OB�`B�fB�yB�yB�yB�B�B�B��B��B��B�B�*B	HB	�B	�B	�B	�B	!�B	%B	$
B	 �B	�B	�B	�B	!�B	'B	3eB	3fB	/NB	-AB	/NB	4lB	L�B	PB	O
B	K�B	NB	]_B	_lB	`rB	`rB	b~B	e�B	m�B	q�B	q�B	q�B	q�B	s�B	t�B	u�B	v�B	v�B	w�B	w�B	r�B	n�B	m�B	n�B	q�B	w�B	z	B	|B	�.B	�.B	w�B	t�B	t�B	w�B	z
B	{B	}B	�4B	�MB	�rB	�~B	�xB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�B	�B	�B	�"B	�(B	�.B	�.B	�.B	�MB	�YB	�eB	�xB	�~B	��B	��B	��B	��B	��B	��B	»B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�	B	�B	�G�O�B	�GB	�B	��B
	�B
SB
rB
mB
&?B
+�B
2�B
8�B
<(B
CB
I]B
OhB
R�B
W�B
\NB
bYB
h/B
j�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111141111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111  <#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
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
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
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
PRES            TEMP            PSAL            PRES_ADJUSTED = PRES - dP                                                                                                                                                                                                                                       none                                                                                                                                                                                                                                                            PSAL_ADJUSTED = PSAL + dS, where dS is calculated from a potential conductivity (ref to 0 dbar) multiplicative adjustment factor r.                                                                                                                             dP =0.3 dbar.                                                                                                                                                                                                                                                   none                                                                                                                                                                                                                                                            r =0.9998(+/-0.0001), vertically averaged dS =-0.008(+/-0.005) in PSS-78.                                                                                                                                                                                       Pressures adjusted by using pressure offset at the sea surface. The quoted error is manufacturer specified accuracy in dbar.                                                                                                                                    The quoted error is manufacturer specified accuracy with respect to ITS-90 at time of laboratory calibration.                                                                                                                                                   Significant salinity drift detected. OWC least squares fit adopted. Salinity also adjusted for effects of pressure adjustment. The quoted error is max[0.01, 1xOWC uncertainty] in PSS-78.                                                                      202202041144202022020411442020220204114420  AO  ARCAADJP                                                                    20200618141401    IP                G�O�G�O�G�O�                AO  ARGQQCPL                                                                    20200618141401  QCP$                G�O�G�O�G�O�5F03E           AO  ARGQQCPL                                                                    20200618141401  QCF$                G�O�G�O�G�O�0               UW  ARSQUWQC    WOD & nearby Argo as visual check                               20220204114420  IP                  G�O�G�O�G�O�                