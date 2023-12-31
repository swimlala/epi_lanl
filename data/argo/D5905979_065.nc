CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             
   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       2020-06-18T14:13:54Z creation      
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
_FillValue                    ��   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                    ��   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                    ��   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                    ��   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  @  ��   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                    �0   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                    �@   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    �D   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar         �T   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar         �X   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�        �\   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    �`Argo profile    3.1 1.2 19500101000000  20200618141354  20220204114417  5905979 UW, Argo                                                        STEPHEN RISER                                                   PRES            TEMP            PSAL               AA   AO  7662                            2C  D   APEX                            8312                            080318                          846 @د\�ck�1   @د]O��@6w
=p���d-1   GPS     Primary sampling: mixed [deeper than nominal 985dbar: discrete; nominal 985dbar to surface: 2dbar-bin averaged]                                                                                                                                                    AA   B   B   @���@�  A   A   A@  A`  A�  A�  A�  A�  A�  A�  A�  A�  B   B  B  B  B   B(  B0  B8  B@  BH  BP  BX  B`  Bh  Bp  Bx  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  C   C  C  C  C  C
  C  C  C  C  C�fC  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Ce�fCh  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C��3C�  C�  C�  C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D��D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D��D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,fD,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DJ��DK� DL  DL� DM  DM� DN  DN� DO  DO� DO��DPy�DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dly�Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dty�Dy��D�%�D�XRD��\D�� D��D�Y�D��)D�� D��D�W�D��D���D��D�W
DژRD��=D�&D�R=D� D��f11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   @�33@�ff@�ffA33A;33A[33A{33A���A���A���A���A͙�Aݙ�A홚A���B��B��B��B��B&��B.��B6��B>��BF��BN��BV��B^��Bf��Bn��Bv��B~��B�ffB�ffB�ffB�ffB�ffB�ffB�ffB�ffB�ffB�ffB�ffB�ffB�ffB�ffB�ffB�ffB�ffB�ffB�ffB�ffB�ffB�ffB�ffB�ffB�ffB�ffB�ffB�ffB�ffB�ffB�ffB�ffC�3C�3C�3C�3C	�3C�3C�3C�3C�3C��C�3C�3C�3C�3C�3C�3C!�3C#�3C%�3C'�3C)�3C+�3C-�3C/�3C1�3C3�3C5�3C7�3C9�3C;�3C=�3C?�3CA�3CC�3CE�3CG�3CI�3CK�3CM�3CO�3CQ�3CS�3CU�3CW�3CY�3C[�3C]�3C_�3Ca�3Cc�3Ce��Cg�3Ci�3Ck�3Cm�3Co�3Cq�3Cs�3Cu�3Cw�3Cy�3C{�3C}�3C�3C�ٚC�ٚC�ٚC��gC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC��gC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC���C���C�ٚC�ٚC�ٚC���C�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC��gC�ٚC�ٚC�ٚC��gC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚD l�D ��Dl�D��Dl�D��Dl�D��Dl�D��Dl�D��Dl�D��Dl�D��Dl�D��D	l�D	��D
l�D
��Dl�D��Dl�D��Dl�D��Dl�D�gDl�D��Dl�D��Dl�D��Dl�D��Dl�D��Dl�D��Dl�D��Dl�D��Dl�D��Dl�D��Dl�D��Dl�D��Dl�D�gDl�D��Dl�D��Dl�D��Dl�D��D l�D ��D!l�D!��D"l�D"��D#l�D#��D$l�D$��D%l�D%��D&l�D&��D'l�D'��D(l�D(��D)l�D)��D*l�D*��D+l�D+�3D,l�D,��D-l�D-��D.l�D.��D/l�D/��D0l�D0��D1l�D1��D2l�D2��D3l�D3��D4l�D4��D5l�D5��D6l�D6��D7l�D7��D8l�D8��D9l�D9��D:l�D:��D;l�D;��D<l�D<��D=l�D=��D>l�D>��D?l�D?��D@l�D@��DAl�DA��DBl�DB��DCl�DC��DDl�DD��DEl�DE��DFl�DF��DGl�DG��DHl�DH��DIl�DI��DJl�DJ�gDKl�DK��DLl�DL��DMl�DM��DNl�DN��DOl�DO�gDPfgDP��DQl�DQ��DRl�DR��DSl�DS��DTl�DT��DUl�DU��DVl�DV��DWl�DW��DXl�DX��DYl�DY��DZl�DZ��D[l�D[��D\l�D\��D]l�D]��D^l�D^��D_l�D_��D`l�D`��Dal�Da��Dbl�Db��Dcl�Dc��Ddl�Dd��Del�De��Dfl�Df��Dgl�Dg��Dhl�Dh��Dil�Di��Djl�Dj��Dkl�Dk��DlfgDl��Dml�Dm��Dnl�Dn��Dol�Do��Dpl�Dp��Dql�Dq��Drl�Dr��Dsl�Ds��DtfgDyo\D�)D�N�D���D��fD��D�P D���D��fD�
D�ND��zD��)D�3D�MpDڎ�D���D�zD�H�D�~fD���11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   @��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��A��yA��mA��A��A��A��A��A��A��yA��A��A��A��A��A��A��A��yA��mA��`A��
A�ƨA���A��A��7A�n�A�Q�A�ȴA�A��A��+A�^5A��A��/A���A��A�E�A�1A��A��A�ĜA���A�z�A�bA��wA��+A�C�A��A���A�Q�A��-A��
A�t�A�ffA�dZA�{A���A�(�A��/A��jA���A�G�A��TA�JA��A��DA���A���A�5?A���A�p�A�C�A���A���A�A�A�jA���A�oA�
=A��A�ȴA��A�1'A��^A�?}A��FA���A��A�=qA�S�A�E�A��!A�(�A��DA��A��!A��wA���A�l�A�G�A��A�\)A� �A�M�A�ȴA��uA��^A�ffA�A�n�A���A�A�A�&�A�r�A�A}p�A|bNA{Az1'Ay�hAw�Av�Au�^At��As��Aq��AnI�Ak��Aj�+AioAh  Ac��A`�A\r�AX��AX�RAXr�AU%AO��AMhsAJbAG��AGK�AFAE�AEl�AE7LAD�yAAA@JA?dZA>�9A<�`A;?}A9��A7�#A5x�A3�wA3|�A3`BA333A3"�A3�A2�`A1%A.ffA,��A,��A,z�A+`BA)l�A(-A&��A$�A"n�A!�-A ^5A�AffA�A�A��A�A�7Az�A��AbNAS�A��AbNAoAM�A1A��A�A`BA��Ap�A�AVAt�A	��A�9Az�A��A�AXA�mA��AffA$�At�A ��A ��A �+@��m@�%@��w@��@��h@���@��-@��h@��@��@� �@�\)@��@��@�Z@�C�@�o@�R@�n�@�hs@�z�@�@�K�@��@�$�@�9@��
@�l�@��@�\@��@��@�A�@��;@�
=@�^5@ݲ-@ۅ@���@���@��;@և+@ա�@Ӆ@���@ёh@��@Ͼw@Η�@�{@�?}@�1@���@�@ɩ�@���@ț�@ǶF@���@�n�@�$�@š�@�G�@���@�S�@�~�@�O�@��9@�1'@�\)@���@�j@��+@��@���@��y@�ff@���@���@�p�@�%@��D@�\)@�M�@�x�@���@�z�@�A�@��;@�K�@�V@�+@��y@���@�~�@�V@���@��h@�G�@�&�@���@��@�1'@�dZ@�-@���@���@�`B@��@�%@���@��j@���@�j@�9X@�  @��F@�@��+@�@��T@��h@�`B@��@��D@�(�@��F@��@���@���@�X@�/@��@��@��/@��9@�1@�t�@�33@��@��R@��+@�-@��#@���@��h@�?}@��@�Ĝ@�z�@�A�@��;@��F@���@��P@�dZ@�C�@��R@�M�@�{@�J@��@�@�`B@���@��`@���@��9@��u@�Q�@���@�dZ@�C�@�@�^5@���@��@��w@�\)@�-@���@���@���@�@�{@�$�@�-@�E�@�^5@�n�@��\@���@���@��!@��R@��@���@��@�l�@�t�@�K�@�-@�hs@�-@�$�@��-@���@��j@��u@�Q�@� �@��
@��@���@�|�@�K�@�S�@�+@�"�@��@��+@�v�@�^5@�{@�@��#@���@�?}@�/@�&�@���@�j@�  @�dZ@�\)@�;d@�@���@��\@�^5@�$�@���@�X@��@�&�@�O�@�G�@��@��@��`@�Ĝ@��j@��j@�Ĝ@���@���@��/@��/@��j@�Q�@� �@��@���@�dZ@�
=@��@���@���@�n�@�V@�-@�5?@�$�@��T@~3�@s;d@m^�@c�*@[�g@U�z@NkQ@C�@<�?@6�@1V@+��@&ȴ@"&�@��@�@M@�r@
u%@&�@
=11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111   A��yA��mA��A��A��A��A��A��A��yA��A��A��A��A��A��A��A��yA��mA��`A��
A�ƨA���A��A��7A�n�A�Q�A�ȴA�A��A��+A�^5A��A��/A���A��A�E�A�1A��A��A�ĜA���A�z�A�bA��wA��+A�C�A��A���A�Q�A��-A��
A�t�A�ffA�dZA�{A���A�(�A��/A��jA���A�G�A��TA�JA��A��DA���A���A�5?A���A�p�A�C�A���A���A�A�A�jA���A�oA�
=A��A�ȴA��A�1'A��^A�?}A��FA���A��A�=qA�S�A�E�A��!A�(�A��DA��A��!A��wA���A�l�A�G�A��A�\)A� �A�M�A�ȴA��uA��^A�ffA�A�n�A���A�A�A�&�A�r�A�A}p�A|bNA{Az1'Ay�hAw�Av�Au�^At��As��Aq��AnI�Ak��Aj�+AioAh  Ac��A`�A\r�AX��AX�RAXr�AU%AO��AMhsAJbAG��AGK�AFAE�AEl�AE7LAD�yAAA@JA?dZA>�9A<�`A;?}A9��A7�#A5x�A3�wA3|�A3`BA333A3"�A3�A2�`A1%A.ffA,��A,��A,z�A+`BA)l�A(-A&��A$�A"n�A!�-A ^5A�AffA�A�A��A�A�7Az�A��AbNAS�A��AbNAoAM�A1A��A�A`BA��Ap�A�AVAt�A	��A�9Az�A��A�AXA�mA��AffA$�At�A ��A ��A �+@��m@�%@��w@��@��h@���@��-@��h@��@��@� �@�\)@��@��@�Z@�C�@�o@�R@�n�@�hs@�z�@�@�K�@��@�$�@�9@��
@�l�@��@�\@��@��@�A�@��;@�
=@�^5@ݲ-@ۅ@���@���@��;@և+@ա�@Ӆ@���@ёh@��@Ͼw@Η�@�{@�?}@�1@���@�@ɩ�@���@ț�@ǶF@���@�n�@�$�@š�@�G�@���@�S�@�~�@�O�@��9@�1'@�\)@���@�j@��+@��@���@��y@�ff@���@���@�p�@�%@��D@�\)@�M�@�x�@���@�z�@�A�@��;@�K�@�V@�+@��y@���@�~�@�V@���@��h@�G�@�&�@���@��@�1'@�dZ@�-@���@���@�`B@��@�%@���@��j@���@�j@�9X@�  @��F@�@��+@�@��T@��h@�`B@��@��D@�(�@��F@��@���@���@�X@�/@��@��@��/@��9@�1@�t�@�33@��@��R@��+@�-@��#@���@��h@�?}@��@�Ĝ@�z�@�A�@��;@��F@���@��P@�dZ@�C�@��R@�M�@�{@�J@��@�@�`B@���@��`@���@��9@��u@�Q�@���@�dZ@�C�@�@�^5@���@��@��w@�\)@�-@���@���@���@�@�{@�$�@�-@�E�@�^5@�n�@��\@���@���@��!@��R@��@���@��@�l�@�t�@�K�@�-@�hs@�-@�$�@��-@���@��j@��u@�Q�@� �@��
@��@���@�|�@�K�@�S�@�+@�"�@��@��+@�v�@�^5@�{@�@��#@���@�?}@�/@�&�@���@�j@�  @�dZ@�\)@�;d@�@���@��\@�^5@�$�@���@�X@��@�&�@�O�@�G�@��@��@��`@�Ĝ@��j@��j@�Ĝ@���@���@��/@��/@��j@�Q�@� �@��@���@�dZ@�
=@��@���@���@�n�@�V@�-@�5?@�$�G�O�@~3�@s;d@m^�@c�*@[�g@U�z@NkQ@C�@<�?@6�@1V@+��@&ȴ@"&�@��@�@M@�r@
u%@&�@
=11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111   ;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oG�O�;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oB}�B|�B|�B}�B|�B|�B}�B}�B}�B}�B}�B}�B}�B}�B}�B}�B}�B}�B}�B~�B~�B� B� B� B� B�B�DB��B�-B�jBǮB�
B�/B�NB�mB�B�B��B��B��B  BB1BJBbBoBPBoB{B'�B9XB=qB?}BE�BL�BaHBdZB`BB^5B^5BXBVBjBy�BiyB_;BffBdZBH�B5?B.B%�BbBB�B�B�B�ZB�BɺB�wB�B��B��B��B��B��B�hB�=Bx�Bp�BiyB`BBYBP�B1'BB
��B
��B
��B
�5B
�qB
��B
�uB
�B
t�B
n�B
ffB
W
B
I�B
C�B
5?B
)�B
�B
{B
DB
B	��B	��B	�B	�B	�B	�B	�B	�mB	��B	ŢB	�jB	�-B	�B	��B	�B	m�B	P�B	L�B	H�B	:^B	�B	B�B�BB�)B�B��B��B��B��BƨB�dB�LB�3B�B��B��B��B�7B�B� B� B� B~�B}�B{�B{�Bs�Bo�Bk�BjBiyBiyBdZBcTBaHB[#BXBVBW
BN�BP�BL�BK�BJ�BI�BK�BH�BF�BF�BD�BC�BC�BA�BA�B@�B@�B?}B>wB@�B=qB;dB9XB8RB5?B49B49B33B33B33B1'B0!B0!B1'B/B/B.B/B/B.B.B-B1'B0!B0!B0!B1'B33B5?B7LB8RB?}BB�BD�BF�BG�BJ�BH�BF�BF�BN�BM�BQ�BR�BR�BR�BS�BT�BXB[#B\)B_;B`BB`BB_;B^5B^5B_;Be`BdZBffBhsBhsBiyBk�Bn�Bo�Bp�Bt�Bu�Bx�Bx�B{�B{�B~�B�B�B�B�B�B�%B�=B�=B�PB�\B�hB�{B��B��B��B��B��B��B�B�!B�!B�!B�-B�?B�XB�jB�wB�}B��BBBĜB��B�5B�;B�BB�HB�NB�`B�sB�B�B�B�B��B��B	B	%B	+B		7B	DB	DB	JB	\B	hB	uB	�B	�B	�B	�B	!�B	%�B	%�B	'�B	)�B	-B	.B	1'B	49B	6FB	:^B	;dB	D�B	D�B	D�B	F�B	G�B	I�B	R�B	VB	XB	YB	[#B	\)B	_;B	bNB	dZB	dZB	ffB	hsB	iyB	k�B	m�B	o�B	p�B	p�B	q�B	r�B	s�B	w�B	z�B	� B	�B	�B	�B	�B	�B	�B	�B	�B	�%B	�%B	�7B	�DB	�PB	�VB	�VB	�\B	�hB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�B	�B	�'B	�^B	�wB	�}B	�}B	��B	ǮB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�
B	�
B	�
B	�B	�B	�B	�B	�#B	�)B	�)B	�5B	�;B	�BB	�HB	�HB	�NB	�TB	�NB	�NB	�HB	�BB	�;B	�;B	�BB	�HB	�NB	�NB	�NB	�NB	�TB	�TB	�ZB	�fB	�fB	�mB	�mB	�mB	�yB	�yB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�TB
�B
TB
B
%,B
*KB
3�B
<�B
C�B
I�B
N�B
TaB
X�B
\�B
c:B
f�B
iB
pB
v�B
{B
~�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111   BvBuBuBvBuBuBvBvBvBvBvBvBvBvBvBvBvBvBvBw#Bw#Bx)Bx)Bx)Bx)By/B�mB��B�TB��B��B�.B�SB�rBߑB�B��B��B��B�B�"B�4B SBlB�B
�BrB
�B�B B1xB5�B7�B=�BD�BYfB\wBX`BVSBVSBP/BN#Bb�Bq�Ba�BWZB^�B\yB@�B-aB&7BB�B�,B��B��B��B܃B�GB��B��B�HB�B��B��B��B��B��B�mBqBh�Ba�BXuBQKBIB)^B
�FB
�)B
�B
��B
�rB
��B
��B
��B
|]B
mB
f�B
^�B
OQB
BB
;�B
-�B
"FB
	B
�B
�B	�YB	�5B	�B	��B	��B	��B	�B	��B	߽B	�DB	��B	��B	��B	�iB	��B	y^B	e�B	IAB	E)B	AB	2�B	�B�{B�BاBԎB�jB�XB�KB�EB�:B�B��B��B��B�lB�6B�B��B��BzyBxnBxnBxnBwhBvbBtUBtUBl%BhBc�Bb�Ba�Ba�B\�B[�BY�BS�BP�BNwBO}BGMBIYBEABD;BC6BB/BD<BA)B?B?B=B<B<B9�B9�B8�B8�B7�B6�B8�B5�B3�B1�B0�B-�B,�B,�B+�B+�B+�B)�B(�B(�B)�B'�B'�B&�B'�B'�B&�B&�B%�B)�B(�B(�B(�B)�B+�B-�B/�B0�B7�B;	B=B?"B@(BC;BA.B?"B?"BGSBFMBJfBKlBKlBKlBLrBMxBP�BS�BT�BW�BX�BX�BW�BV�BV�BW�B]�B\�B^�B`�B`�Ba�Bc�BgBhBiBm6Bn=BqOBqOBtaBtaBwtBy�Bz�B{�B|�B}�B~�B��B��B��B��B��B��B��B�%B�DB�bB�oB�uB��B��B��B��B��B��B��B��B��B��B�B�B�B�B�cB֬BײBعBٿB��B��B��B�B�B�B�&B�2B�PB��B��B��B	�B	�B	�B	�B	�B		�B	�B	�B	B	B	2B	?B	VB	VB	 cB	"oB	%�B	&�B	)�B	,�B	.�B	2�B	3�B	=B	=B	=B	?B	@ B	B,B	KcB	NuB	P�B	Q�B	S�B	T�B	W�B	Z�B	\�B	\�B	^�B	`�B	a�B	c�B	fB	hB	iB	iB	jB	k B	l&B	p>B	sPB	xoB	z{B	{�B	|�B	|�B	}�B	}�B	}�B	}�B	~�B	~�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�&B	�EB	�KB	�JB	�PB	�PB	�]B	�]B	�cB	�iB	�oB	�oB	�uB	�uB	�{B	��B	��B	��B	��B	��B	��B	��B	�B	�2B	�8B	�2B	�2B	�2B	�8B	�8B	�JB	�PB	�PB	�WB	�WB	�iB	�iB	�oB	�oB	�uB	�uB	�uB	�{B	�{B	тB	҈B	ӎB	ԔB	ԔB	֟B	ץB	جB	ٲB	ٲB	ڸB	۾B	ڸB	ڸB	ٲB	جB	צB	צB	حB	ٳB	ڸB	ڸB	ڸB	ڸB	۾B	۾B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	��B	�B	�G�O�B	�B	�YB

�B
iB
�B
"�B
,B
5:B
<1B
BVB
GZB
L�B
Q.B
UB
[�B
^�B
avB
hlB
obB
s{B
wE11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111   <#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
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
PRES            TEMP            PSAL            PRES_ADJUSTED = PRES - dP                                                                                                                                                                                                                                       none                                                                                                                                                                                                                                                            PSAL_ADJUSTED = PSAL + dS, where dS is calculated from a potential conductivity (ref to 0 dbar) multiplicative adjustment factor r.                                                                                                                             dP =0.3 dbar.                                                                                                                                                                                                                                                   none                                                                                                                                                                                                                                                            r =0.9998(+/-0.0001), vertically averaged dS =-0.008(+/-0.004) in PSS-78.                                                                                                                                                                                       Pressures adjusted by using pressure offset at the sea surface. The quoted error is manufacturer specified accuracy in dbar.                                                                                                                                    The quoted error is manufacturer specified accuracy with respect to ITS-90 at time of laboratory calibration.                                                                                                                                                   Significant salinity drift detected. OWC least squares fit adopted. Salinity also adjusted for effects of pressure adjustment. The quoted error is max[0.01, 1xOWC uncertainty] in PSS-78.                                                                      202202041144172022020411441720220204114417  AO  ARCAADJP                                                                    20200618141354    IP                G�O�G�O�G�O�                AO  ARGQQCPL                                                                    20200618141354  QCP$                G�O�G�O�G�O�5F03E           AO  ARGQQCPL                                                                    20200618141354  QCF$                G�O�G�O�G�O�0               UW  ARSQUWQC    WOD & nearby Argo as visual check                               20220204114417  IP                  G�O�G�O�G�O�                