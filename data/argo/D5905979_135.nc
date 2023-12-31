CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             
   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       2020-06-19T17:09:26Z creation      
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
_FillValue                    ��Argo profile    3.1 1.2 19500101000000  20200619170926  20220204114424  5905979 UW, Argo                                                        STEPHEN RISER                                                   PRES            TEMP            PSAL               �A   AO  7662                            2C  D   APEX                            8312                            080318                          846 @�݉9�1   @��""1 @7
=p���b���l�D1   GPS     Primary sampling: mixed [deeper than nominal 985dbar: discrete; nominal 985dbar to surface: 2dbar-bin averaged]                                                                                                                                                    �A   B   B   @���@�  @���A   A@  A`  A�  A�  A�  A�  A�  A�  A�  A�  B   BffB  B  B   B(  B0  B8  B@  BH  BP  BX  B`  Bh  Bp  Bx  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�33B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  C   C  C  C  C�fC
  C  C  C  C  C�fC  C  C�fC  C  C   C"  C$  C&  C(�C*  C,  C.  C0  C2  C4  C6  C8�C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Ck�fCm�fCp  Cr  Ct  Cv  Cx  Cz  C|  C}�fC�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C��3C�  C�  C�  C�  C�  C�  C�  C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� DfD� D  D� D	  D	� D
  D
� D  D� D  D� D��Dy�D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D�fD  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&�fD'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dt� Dt� Dy��D�${D�Y�D��D���D� �D�_
D���D��{D�%qD�[3D��D��D��D�W�Dڛ�D��=D�
D�[3D�D���111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  @��H@�{@��HA
=A;
=A[
=A{
=A��A��A��A��AͅA݅A�A��B(�BBBB&B.B6B>BFBNBVB^BfBnBvB~B�aHB�aHB�aHB�aHB�aHB�aHB�aHB�aHB�aHB�aHB�aHB�aHB�aHB��{B�aHB�aHB�aHB�aHB�aHB�aHB�aHB�aHB�aHB�aHB�aHB�aHB�aHB�aHB�aHB�aHB�aHB�aHC��C��C��C�
C	��C��C��C��C��C�
C��C��C�
C��C��C��C!��C#��C%��C'�>C)��C+��C-��C/��C1��C3��C5��C7�>C9��C;��C=��C?��CA��CC��CE��CG��CI��CK��CM��CO��CQ��CS��CU��CW��CY��C[��C]��C_��Ca��Cc��Ce��Cg��Ci��Ck�
Cm�
Co��Cq��Cs��Cu��Cw��Cy��C{��C}�
C��C��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��C��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC�˅C�˅C��RC��RC��RC��RC��RC��RC��RC�˅C��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RD l)D �)Dl)D�)Dl)D�)Dl)D�)Dl)D�)Dl)D�)Dl)D�Dl)D�)Dl)D�)D	l)D	�)D
l)D
�)Dl)D�)Dl)D��De�D�)Dl)D�)Dl)D�)Dl)D�)Dl)D�)Dl)D�)Dl)D�)Dl)D�)Dl)D�)Dl)D�)Dl)D�)Dr�D�)Dl)D�)Dl)D�)Dl)D�)Dl)D�)Dl)D�)Dl)D�)Dl)D�)D l)D �)D!l)D!�)D"l)D"�)D#l)D#�)D$l)D$�)D%l)D%�)D&r�D&�)D'l)D'�)D(l)D(�)D)l)D)�)D*l)D*�)D+l)D+�)D,l)D,�)D-l)D-�)D.l)D.�)D/l)D/�)D0l)D0�)D1l)D1�)D2l)D2�)D3l)D3�)D4l)D4�)D5l)D5�)D6l)D6�)D7l)D7�)D8l)D8�)D9l)D9�)D:l)D:�)D;l)D;�)D<l)D<�)D=l)D=�)D>l)D>�)D?l)D?�)D@l)D@�)DAl)DA�)DBl)DB�)DCl)DC�)DDl)DD�)DEl)DE�)DFl)DF�)DGl)DG�)DHl)DH�)DIl)DI�)DJl)DJ�)DKl)DK�)DLl)DL�)DMl)DM�)DNl)DN�)DOl)DO�)DPl)DP�)DQl)DQ�)DRl)DR�)DSl)DS�)DTl)DT�)DUl)DU�)DVl)DV�)DWl)DW�)DXl)DX�)DYl)DY�)DZl)DZ�)D[l)D[�)D\l)D\�)D]l)D]�)D^l)D^�)D_l)D_�)D`l)D`�)Dal)Da�)Dbl)Db�)Dcl)Dc�)Ddl)Dd�)Del)De�)Dfl)Df�)Dgl)Dg�)Dhl)Dh�)Dil)Di�)Djl)Dj�)Dkl)Dk�)Dll)Dl�)Dml)Dm�)Dnl)Dn�)Dol)Do�)Dpl)Dp�)Dql)Dq�)Drl)Dr�)Dsl)Ds�)Dtl)Dt�)Dyq�D��D�O�D�{3D���D�
D�UD���D���D��D�QGD��3D���D��D�M�Dڑ�D��QD�D�QGD�3D�� 111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  @��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��A�A�A�K�A�K�A�Q�A�Q�A�O�A�Q�A�Q�A�Q�A�Q�A�S�A�Q�A�VA�VA�S�A�S�A�Q�A�=qA�ĜA��A��A��A�1'A�
=A�  A�JA��A��#A�|�A�`BA�S�A�G�A�;dA�-A��A��;A���A��7A�p�A�7LA�{A�l�A�G�A�ffA�5?A�{A��RA���A�ĜA�5?A�%A��yA��;A��A�{A���A�7LA�+A�
=A�E�A���A�
=A��yA��`A���A�(�A���A�7LA��FA�5?A���A�A�9XA��A�v�A�{A��/A�C�A�1'A�K�A�I�A���A��A�`BA��A�jA�XA�9XA�ĜA�bNA� �A��RA��A�VA��yA��FA�I�A��\A�?}AG�A}dZA{�
A{VAz�AxȴAvv�At��As�ArAp�!ApffAo�#AoAm�wAjJAgƨAfȴAfAe?}Ac��Ab�HAa��AaO�A_�-A^ZA]�A[��AYƨAY&�AW�AV �AU�ATjASƨAR�AP�AM��AJr�AG�;AF�`AE�AE+AC/A?�7A<�uA;�wA:��A8�`A6��A4�A333A1�
A0�A0r�A/�-A.�!A-+A,{A+�PA*�A(5?A'A$��A"�uA!��A �A �uA VA (�A�+A��A�A"�AM�AVA^5A1'AJA��AJA�\A�-A�AXAE�A�HA{AbNA��A
�`A	�
A	�A��A�
A�9Ax�A�jAn�A$�A�A�-Ax�A7LA��A ��A A�@��@�=q@��@�{@��D@�ff@�hs@�hs@���@�(�@�|�@�=q@�I�@�v�@��@���@��@�@��@�  @�@ᙚ@ߥ�@�hs@�b@�S�@�=q@�p�@�1@�+@���@�%@���@�;d@���@�l�@���@�{@��@���@�@�V@�Ĝ@͑h@�?}@��/@�b@�1@���@˶F@�dZ@�C�@��y@�~�@�5?@�{@��@ə�@�Q�@��H@��/@�b@��@�ƨ@�|�@��@�~�@�5?@�{@��-@���@�z�@�;d@���@�^5@�`B@��D@�(�@��@�|�@��\@�/@��u@�1'@��y@��T@��@�9X@��\@�Ĝ@��@��@�M�@�@���@��@�J@���@���@��h@�G�@��D@�bN@��@�;d@���@�n�@�V@�=q@���@��h@�@�@�dZ@���@�(�@�"�@���@�ȴ@�ȴ@��!@��+@��@�7L@�?}@���@��@�X@�p�@�V@�V@���@���@��!@�G�@�Ĝ@�I�@�7L@��`@���@�p�@��@�&�@�7L@�/@�dZ@���@���@��@�^5@�@��y@�J@�1@���@���@�M�@��@�Ĝ@��F@�
=@�E�@��@��#@�p�@�&�@�?}@�X@��@�/@��D@���@��
@��@��/@��@�1'@�b@��F@�"�@�{@���@�@���@��^@�p�@�hs@�`B@�?}@�%@��`@�bN@��H@�S�@��w@���@�33@��+@�E�@��#@�p�@�%@���@�z�@�Q�@��@��m@��;@�ƨ@��F@�l�@�+@�"�@��R@�^5@�5?@��@���@�hs@�%@��@� �@��m@��
@��F@���@�C�@��@��!@�ff@�-@�@��@��7@��@���@�Ĝ@���@�j@�bN@�A�@�(�@�1@��
@���@�|�@�C�@�+@�o@�
=@�
=@���@�v�@�V@�E�@��@�J@�J@��@���@�G�@�/@��@�V@���@���@�j@� �@���@��w@���@�\)@�"�@���@���@�~�@�^5@�V@�M�@�=q@�J@�J@��@���@x�U@q�@f�@b=q@]Vm@V+k@O;d@Jv�@A�D@:Q@3��@.��@)Y�@#��@v`@�T@�#@@�@x111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111  A�A�A�K�A�K�A�Q�A�Q�A�O�A�Q�A�Q�A�Q�A�Q�A�S�A�Q�A�VA�VA�S�A�S�A�Q�A�=qA�ĜA��A��A��A�1'A�
=A�  A�JA��A��#A�|�A�`BA�S�A�G�A�;dA�-A��A��;A���A��7A�p�A�7LA�{A�l�A�G�A�ffA�5?A�{A��RA���A�ĜA�5?A�%A��yA��;A��A�{A���A�7LA�+A�
=A�E�A���A�
=A��yA��`A���A�(�A���A�7LA��FA�5?A���A�A�9XA��A�v�A�{A��/A�C�A�1'A�K�A�I�A���A��A�`BA��A�jA�XA�9XA�ĜA�bNA� �A��RA��A�VA��yA��FA�I�A��\A�?}AG�A}dZA{�
A{VAz�AxȴAvv�At��As�ArAp�!ApffAo�#AoAm�wAjJAgƨAfȴAfAe?}Ac��Ab�HAa��AaO�A_�-A^ZA]�A[��AYƨAY&�AW�AV �AU�ATjASƨAR�AP�AM��AJr�AG�;AF�`AE�AE+AC/A?�7A<�uA;�wA:��A8�`A6��A4�A333A1�
A0�A0r�A/�-A.�!A-+A,{A+�PA*�A(5?A'A$��A"�uA!��A �A �uA VA (�A�+A��A�A"�AM�AVA^5A1'AJA��AJA�\A�-A�AXAE�A�HA{AbNA��A
�`A	�
A	�A��A�
A�9Ax�A�jAn�A$�A�A�-Ax�A7LA��A ��A A�@��@�=q@��@�{@��D@�ff@�hs@�hs@���@�(�@�|�@�=q@�I�@�v�@��@���@��@�@��@�  @�@ᙚ@ߥ�@�hs@�b@�S�@�=q@�p�@�1@�+@���@�%@���@�;d@���@�l�@���@�{@��@���@�@�V@�Ĝ@͑h@�?}@��/@�b@�1@���@˶F@�dZ@�C�@��y@�~�@�5?@�{@��@ə�@�Q�@��H@��/@�b@��@�ƨ@�|�@��@�~�@�5?@�{@��-@���@�z�@�;d@���@�^5@�`B@��D@�(�@��@�|�@��\@�/@��u@�1'@��y@��T@��@�9X@��\@�Ĝ@��@��@�M�@�@���@��@�J@���@���@��h@�G�@��D@�bN@��@�;d@���@�n�@�V@�=q@���@��h@�@�@�dZ@���@�(�@�"�@���@�ȴ@�ȴ@��!@��+@��@�7L@�?}@���@��@�X@�p�@�V@�V@���@���@��!@�G�@�Ĝ@�I�@�7L@��`@���@�p�@��@�&�@�7L@�/@�dZ@���@���@��@�^5@�@��y@�J@�1@���@���@�M�@��@�Ĝ@��F@�
=@�E�@��@��#@�p�@�&�@�?}@�X@��@�/@��D@���@��
@��@��/@��@�1'@�b@��F@�"�@�{@���@�@���@��^@�p�@�hs@�`B@�?}@�%@��`@�bN@��H@�S�@��w@���@�33@��+@�E�@��#@�p�@�%@���@�z�@�Q�@��@��m@��;@�ƨ@��F@�l�@�+@�"�@��R@�^5@�5?@��@���@�hs@�%@��@� �@��m@��
@��F@���@�C�@��@��!@�ff@�-@�@��@��7@��@���@�Ĝ@���@�j@�bN@�A�@�(�@�1@��
@���@�|�@�C�@�+@�o@�
=@�
=@���@�v�@�V@�E�@��@�J@�J@��@���@�G�@�/@��@�V@���@���@�j@� �@���@��w@���@�\)@�"�@���@���@�~�@�^5@�V@�M�@�=q@�J@�JG�O�@���@x�U@q�@f�@b=q@]Vm@V+k@O;d@Jv�@A�D@:Q@3��@.��@)Y�@#��@v`@�T@�#@@�@x111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111  ;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oG�O�;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oB�B!�B �B"�B#�B#�B#�B#�B"�B"�B"�B$�B#�B"�B"�B"�B"�B%�B8RB��B��B�HB�TB�yB��BJ�Bm�B�%B��B��B��B��B��B��B��B��B�'B�3B�?B�RB�XB�3B��B��B�\Bt�BcTBW
BO�BW
BXB]/BW
BO�BG�BF�BB�BA�B?}B9XB49B33B1'B0!B0!B1'B%�B�B�BPBB��B�/B��BɺB��B�dB�-B��B�7Bu�BhsBW
BA�B/B�B
=B
��B
�B
�sB
�TB
�B
��B
�-B
��B
�%B
|�B
p�B
ZB
M�B
@�B
5?B
,B
'�B
�B
	7B	��B	�B	�ZB	�)B	�
B	��B	��B	ŢB	�B	��B	�oB	�PB	�%B	~�B	s�B	m�B	iyB	`BB	YB	R�B	N�B	@�B	;dB	49B	.B	'�B	$�B	�B	�B	hB	B�B�B��B��BȴB�qB�B��B��B��B�oB�\B�1B�B{�Bx�Bv�Bu�Bt�Bw�Bv�Bt�Bs�Bp�Bl�Bl�Be`BbNB_;B^5B]/B[#BZBT�BM�BI�BG�BF�BG�BH�BH�BI�BL�BL�BJ�BI�BG�BG�B@�B?}B:^B49B0!B-B,B)�B(�B'�B%�B$�B#�B#�B"�B"�B"�B!�B"�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B �B �B!�B#�B#�B#�B%�B$�B#�B#�B$�B"�B �B �B!�B#�B%�B%�B#�B$�B&�B(�B%�B'�B+B/B/B0!B0!B1'B5?B@�BE�BH�BK�BK�BL�BR�BXBYBZB[#B[#B[#B[#B[#B[#B[#B`BBbNBcTBcTBdZBe`BffBiyBjBm�Bp�Bq�Bu�Bv�Bu�Bv�Bu�Bt�Bt�Bt�Bx�Bz�B|�B� B�B�B�B�B�B�B�B�=B�VB�oB�{B��B��B��B��B��B�B�!B�'B�3B�FB�RB�^B�jB��BȴB��B�
B�;B�NB�fB�B�B�B�B�B�B��B��B��B��B	  B	B	%B		7B	PB	oB	�B	%�B	(�B	(�B	(�B	)�B	7LB	8RB	9XB	B�B	C�B	F�B	G�B	G�B	H�B	F�B	I�B	J�B	YB	`BB	cTB	dZB	cTB	]/B	]/B	]/B	^5B	_;B	]/B	_;B	bNB	cTB	cTB	cTB	dZB	iyB	k�B	m�B	p�B	q�B	u�B	q�B	z�B	|�B	y�B	|�B	~�B	~�B	|�B	|�B	z�B	|�B	}�B	~�B	�B	�B	�1B	�PB	�VB	�VB	�VB	�hB	��B	��B	��B	��B	��B	��B	�B	�B	�B	�-B	�-B	�-B	�9B	�?B	�FB	�LB	�LB	�^B	�jB	�qB	�wB	�}B	��B	��B	��B	ŢB	ȴB	ȴB	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�)B	�)B	�5B	�;B	�BB	�BB	�BB	�BB	�HB	�NB	�NB	�TB	�ZB	�`B	�`B	�`B	�`B	�sB	�sB	�sB	�yB	�yB	�yB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B
�B
,B
_B
$@B
+QB
3B
9rB
@�B
D�B
K�B
QNB
ZkB
^OB
`vB
d&B
g�B
l�B
p�B
r�B
x�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111  BFB_BYBdBjBjBjBjBdBdBdBpBjBdBdBdBdBvB*�B�XB�{B��B��B��B�DB=9B`Bx�B�B�"B�.B�5B�;B�FB�FB�kB��B��B��B��B��B��B�GB�/B��Bg2BU�BI�BBZBI�BJ�BO�BI�BB[B:,B9&B5B4B1�B+�B&�B%�B#�B"�B"�B#�BfB<BB��B��B�iBϻBǋB�HB�B��B��B�dB{�Bh[B[BI�B4(B!�BUB
��B
�B
�AB
�B
��B
��B
��B
��B
�YB
x�B
o�B
c\B
L�B
@�B
3AB
'�B
�B
�B
VB	��B	��B	�wB	�#B	��B	��B	��B	��B	�oB	��B	�~B	�BB	�$B	x�B	q�B	f�B	`iB	\RB	SB	K�B	E�B	A�B	3aB	.CB	'B	 �B	�B	�B	�B	�B	MB��B�B�B��B��B��B�aB�B��B��B��B�eB�SB{)BvBn�Bk�Bi�Bh�Bg�Bj�Bi�Bg�Bf�Bc�B_�B_�BX`BUNBR;BQ6BP0BN$BMBH B@�B<�B:�B9�B:�B;�B;�B<�B?�B?�B=�B<�B:�B:�B3�B2�B-fB'BB#+B BBBB�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B	B�BBB".B".B#3B#3B$9B(QB3�B8�B;�B>�B>�B?�BFBKBL&BM,BN2BN2BN2BN2BN2BN2BN2BSQBU]BVcBVcBWhBXnBYtB\�B]�B`�Bc�Bd�Bh�Bi�Bh�Bi�Bh�Bg�Bg�Bg�Bk�Bm�Bo�BsBx,Bx,Bv BuBuBw'Bx-B}KB�cB�|B��B��B��B�B�B�B�B�+B�1B�=B�PB�\B�gB�sB��B��B��B�B�AB�SB�kB݃B�B�B�B�B�B��B��B��B��B�B�	B�'B�9B	 QB	pB	�B	�B	�B	�B	�B	�B	*IB	+OB	,UB	5�B	6�B	9�B	:�B	:�B	;�B	9�B	<�B	=�B	LB	S;B	VMB	WSB	VMB	P)B	P)B	P)B	Q/B	R5B	P)B	R5B	UHB	VNB	VNB	VNB	WTB	\sB	^~B	`�B	c�B	d�B	h�B	d�B	m�B	o�B	l�B	o�B	q�B	q�B	o�B	o�B	m�B	o�B	p�B	q�B	s�B	wB	{(B	�FB	�LB	�LB	�LB	�^B	�}B	��B	��B	��B	��B	��B	��B	�B	�B	� B	� B	� B	�,B	�2B	�9B	�?B	�?B	�QB	�\B	�cB	�iB	�oB	�uB	�{B	�{B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	� B	� B	�B	�B	�B	�%B	�+B	�2B	�2B	�2B	�2B	�8B	�>B	�>B	�CB	�IB	�OB	�OB	�OB	�OB	�bB	�bB	�bB	�hB	�hB	�hB	�nB	�nB	�tB	�zB	��B	��B	��B	��B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��G�O�B	�B	��B
B
KB
+B
<B
&B
,\B
3�B
7kB
>�B
D7B
MSB
Q7B
S^B
WB
Z�B
_sB
c�B
e}B
k�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111  <#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
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
PRES            TEMP            PSAL            PRES_ADJUSTED = PRES - dP                                                                                                                                                                                                                                       none                                                                                                                                                                                                                                                            PSAL_ADJUSTED = PSAL + dS, where dS is calculated from a potential conductivity (ref to 0 dbar) multiplicative adjustment factor r.                                                                                                                             dP =0.31 dbar.                                                                                                                                                                                                                                                  none                                                                                                                                                                                                                                                            r =0.9997(+/-0.0001), vertically averaged dS =-0.013(+/-0.005) in PSS-78.                                                                                                                                                                                       Pressures adjusted by using pressure offset at the sea surface. The quoted error is manufacturer specified accuracy in dbar.                                                                                                                                    The quoted error is manufacturer specified accuracy with respect to ITS-90 at time of laboratory calibration.                                                                                                                                                   Significant salinity drift detected. OWC least squares fit adopted. Salinity also adjusted for effects of pressure adjustment. The quoted error is max[0.01, 1xOWC uncertainty] in PSS-78.                                                                      202202041144242022020411442420220204114424  AO  ARCAADJP                                                                    20200619170926    IP                G�O�G�O�G�O�                AO  ARGQQCPL                                                                    20200619170926  QCP$                G�O�G�O�G�O�5F03E           AO  ARGQQCPL                                                                    20200619170926  QCF$                G�O�G�O�G�O�0               UW  ARSQUWQC    WOD & nearby Argo as visual check                               20220204114424  IP                  G�O�G�O�G�O�                