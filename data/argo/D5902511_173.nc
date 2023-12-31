CDF      
      	DATE_TIME         STRING2       STRING4       STRING8       STRING16      STRING32       STRING64   @   	STRING256         N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY                title         Argo float vertical profile    institution       #Scripps Institution of Oceanography    source        
Argo float     history       :2021-06-17T17:24:51Z creation; 2022-02-04T23:30:01Z DMQC;      
references        (http://www.argodatamgt.org/Documentation   comment       	free text      user_manual_version       3.10   Conventions       Argo-3.1 CF-1.6    featureType       trajectoryProfile      comment_on_dac_decoder_version        $Decoded by SIO: Argo SIO SOLOII V2.2   comment_dmqc_operator         bPRIMARY | https://orcid.org/0000-0003-0805-6570 | John Gilson, Scripps Institution of Oceanography        @   	DATA_TYPE                  	long_name         	Data type      conventions       Argo reference table 1     
_FillValue                    7�   FORMAT_VERSION                 	long_name         File format version    
_FillValue                    7�   HANDBOOK_VERSION               	long_name         Data handbook version      
_FillValue                    7�   REFERENCE_DATE_TIME                 	long_name         !Date of reference for Julian days      conventions       YYYYMMDDHHMISS     
_FillValue                    7�   DATE_CREATION                   	long_name         Date of file creation      conventions       YYYYMMDDHHMISS     
_FillValue                    7�   DATE_UPDATE                 	long_name         Date of update of this file    conventions       YYYYMMDDHHMISS     
_FillValue                    7�   PLATFORM_NUMBER                   	long_name         Float unique identifier    conventions       WMO float identifier : A9IIIII     
_FillValue                    8   PROJECT_NAME                  	long_name         Name of the project    
_FillValue                  �  8   PI_NAME                   	long_name         "Name of the principal investigator     
_FillValue                  �  8�   STATION_PARAMETERS           	            	long_name         ,List of available parameters for the station   conventions       Argo reference table 3     
_FillValue                  `  9   CYCLE_NUMBER               	long_name         Float cycle number     conventions       =0...N, 0 : launch cycle (if exists), 1 : first complete cycle      
_FillValue         ��        9x   	DIRECTION                  	long_name         !Direction of the station profiles      conventions       -A: ascending profiles, D: descending profiles      
_FillValue                    9�   DATA_CENTRE                   	long_name         .Data centre in charge of float data processing     conventions       Argo reference table 4     
_FillValue                    9�   DC_REFERENCE                  	long_name         (Station unique identifier in data centre   conventions       Data centre convention     
_FillValue                  @  9�   DATA_STATE_INDICATOR                  	long_name         1Degree of processing the data have passed through      conventions       Argo reference table 6     
_FillValue                    9�   	DATA_MODE                  	long_name         Delayed mode or real time data     conventions       >R : real time; D : delayed mode; A : real time with adjustment     
_FillValue                    9�   PLATFORM_TYPE                     	long_name         Type of float      conventions       Argo reference table 23    
_FillValue                  @  9�   FLOAT_SERIAL_NO                   	long_name         Serial number of the float     
_FillValue                  @  :   FIRMWARE_VERSION                  	long_name         Instrument firmware version    
_FillValue                  @  :T   WMO_INST_TYPE                     	long_name         Coded instrument type      conventions       Argo reference table 8     
_FillValue                    :�   JULD               	long_name         ?Julian day (UTC) of the station relative to REFERENCE_DATE_TIME    standard_name         time   units         "days since 1950-01-01 00:00:00 UTC     conventions       8Relative julian days with decimal part (as parts of day)   axis      T      
resolution        >�E�r�_K   
_FillValue        A.�~            :�   JULD_QC                	long_name         Quality on date and time   conventions       Argo reference table 2     
_FillValue                    :�   JULD_LOCATION                  	long_name         @Julian day (UTC) of the location relative to REFERENCE_DATE_TIME   units         "days since 1950-01-01 00:00:00 UTC     conventions       8Relative julian days with decimal part (as parts of day)   
resolution        ?F�k"kmj   
_FillValue        A.�~            :�   LATITUDE               	long_name         &Latitude of the station, best estimate     standard_name         latitude   units         degree_north   	valid_min         �V�        	valid_max         @V�        axis      Y      
_FillValue        @�i�            :�   	LONGITUDE                  	long_name         'Longitude of the station, best estimate    standard_name         	longitude      units         degree_east    	valid_min         �f�        	valid_max         @f�        axis      X      
_FillValue        @�i�            :�   POSITION_QC                	long_name         ,Quality on position (latitude and longitude)   conventions       Argo reference table 2     
_FillValue                    :�   POSITIONING_SYSTEM                    	long_name         Positioning system     
_FillValue                    :�   PROFILE_PRES_QC                	long_name         #Global quality flag of PRES profile    conventions       Argo reference table 2a    
_FillValue                    :�   PROFILE_TEMP_QC                	long_name         #Global quality flag of TEMP profile    conventions       Argo reference table 2a    
_FillValue                    :�   PROFILE_PSAL_QC                	long_name         #Global quality flag of PSAL profile    conventions       Argo reference table 2a    
_FillValue                    :�   VERTICAL_SAMPLING_SCHEME                  	long_name         Vertical sampling scheme   conventions       Argo reference table 16    
_FillValue                    ;    CONFIG_MISSION_NUMBER                  	long_name         :Unique number denoting the missions performed by the float     conventions       !1...N, 1 : first complete mission      
_FillValue         ��        =    PRES         
      
   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     units         decibar    	valid_min                	valid_max         F;�    C_format      %7.2f      FORTRAN_format        F7.2   
resolution        =#�
   axis      Z      
_FillValue        G�O�     �  =   PRES_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  \�   PRES_ADJUSTED            
      
   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     units         decibar    	valid_min                	valid_max         F;�    C_format      %7.2f      FORTRAN_format        F7.2   
resolution        =#�
   axis      Z      
_FillValue        G�O�     �  d|   PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     units         decibar    C_format      %7.2f      FORTRAN_format        F7.2   
resolution        =#�
   
_FillValue        G�O�     �  ��   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o   
_FillValue        G�O�     �  ��   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o   
_FillValue        G�O�     �  ��   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     units         degree_Celsius     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o   
_FillValue        G�O�     �  �h   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    	valid_min         @      	valid_max         B$     C_format      %10.4f     FORTRAN_format        F10.4      
resolution        9Q�   
_FillValue        G�O�     � �   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 � 9�   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    	valid_min         @      	valid_max         B$     C_format      %10.4f     FORTRAN_format        F10.4      
resolution        9Q�   
_FillValue        G�O�     � Al   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 � `�   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     units         psu    C_format      %10.4f     FORTRAN_format        F10.4      
resolution        9Q�   
_FillValue        G�O�     � h�   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  ` �p   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                   ��   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                   ��   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                   ��   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  T ��   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                   �$   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                   �,   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                   �4   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                   �<   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  � �D   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                   ��   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                   ��   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    ��   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar        �   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar        �   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�       �   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    � Argo profile    3.1 1.2 19500101000000  20210617172451  20220204223514  5902511 5902511 US ARGO PROJECT                                                 US ARGO PROJECT                                                 DEAN ROEMMICH                                                   DEAN ROEMMICH                                                   PRES            TEMP            PSAL            PRES            TEMP            PSAL               �   �AA  AOAO6810_008521_173                 6810_008521_173                 2C  2C  DD  SOLO_II                         SOLO_II                         8521                            8521                            V2.2; SBE602 27Jul16            V2.2; SBE602 27Jul16            853 853 @�}'��Z@�}'��Z11  @�}'>�6z@�}'>�6z@1��D��@1��D���e"�x?��e"�x?�11  GPS     GPS     BA  BA  FF  Primary sampling: averaged [nominal   2 dbar binned data sampled at 1.0 Hz from a SBE41CP; bin detail from 0 dbar (number bins/bin width):   10/  1; 500/  2;remaining/  2]                                                                                     Near-surface sampling: discrete, pumped [data sampled at 0.5Hz from the same SBE41CP]                                                                                                                                                                                 ?�  ?��H@@  @�  @��R@�G�@�G�A   A{A\)A,��AAG�A`  A�Q�A��A�  A�  A�  A�Q�A�Q�A�Q�A��B  B  B(�B (�B((�B0  B8(�B@(�BH  BPQ�BX(�B_�Bh  Bp  Bx  B�  B�{B�  B�  B�(�B�  B�(�B�(�B�  B�{B�  B�  B�  B��B��
B��B�{B�{B�  B�{B�{B�  B�  B�(�B�{B�{B�{B�  B�  B�  B�  B�  C 
=C
=C  C  C
=C

=C  C��C��C{C{C  C��C�HC  C�C {C"
=C$
=C&
=C'�C)�C+�C-��C0  C1��C3�C5��C8  C:  C<  C>
=C@{CB  CD  CF  CG��CJ
=CL  CM��CO��CR
=CT  CV  CW��CY��C[��C]��C`  Cb
=Cd  Cf
=Ch  Ci��Ck�Cn  Cp  Cq��Ct
=Cv  Cx  Cz  C|  C~
=C�  C�  C�C�C�C���C�  C�
=C�C���C�  C���C���C�  C���C�  C�  C�C�  C�C�  C�  C�  C�  C�C�  C�  C�  C�  C�C�  C�  C���C�  C�  C�  C�  C�C�  C�C���C�  C�  C�C�C�C�  C�C�C���C���C�C�C�  C�C�  C��C���C�C���C�  C���C�C�  C�  C���C�C�C���C��C���C�C�C�C�  C�  C�
=C�C���C���C�  C�  C�C�
=C�C���C���C���C�C���C���C���C�C�C�C�
=C�
=C�C���C���C���C���C�  C���C���C���C�  C�C�
=C�C���C���C���C�  C�C�  C�C�
=C�C�C�C�C�C�C�  C���C���C��D   D � D �qD}qD��D}qD�qD� D�qD� D  D��D�D� D�qD}qD  D� D	  D	� D
�D
��D
��DxRD��D}qD  D}qD  D}qD�qD}qD��D� D�D��D�qD� D  D� D�D}qD  D� D�D��D  D��D�D� D�qD��DD��DD�D�D� D  D}qD�qD� D�qD� D   D }qD!  D!� D"  D"� D"�qD#� D$�D$� D%  D%��D%�qD&}qD'  D'� D(  D(��D)  D)� D)�qD*� D+D+��D+�qD,z�D,��D-}qD.  D.� D/�D/��D0  D0}qD0�qD1}qD2  D2� D3  D3z�D3�qD4��D5  D5z�D6  D6� D6�qD7� D7�qD8� D9  D9}qD9�qD:}qD:�qD;� D<  D<��D=�D=z�D=�qD>}qD?  D?}qD?�qD@��DA  DA}qDB  DB� DB�qDC}qDD  DD� DE  DE� DF  DF� DF��DG� DHDH� DH�qDI� DJ  DJ� DK�DK��DL  DL� DM�DM}qDM�qDN� DO�DO��DP  DP� DQ  DQ��DR  DR}qDR�qDS}qDS�qDT}qDU  DU}qDU�qDV� DWDW��DW�qDX� DY  DY� DY�qDZ}qD[  D[}qD\  D\}qD]�D]�D^  D^}qD^��D_}qD`�D`��Da�Da� Db  Db��Dc  Dc��Dd  Dd}qDe  De� Df�Df� DgDg�Dh  Dh� Di�Di��Dj�Dj��Dk�Dk��DlDl� Dl�qDm��Dn  Dn}qDo�Do��Dp�Dp� Dq  Dq� Dr�Dr� Ds  Ds��Dt�Dt}qDu  Du��Dv  Dv}qDv��Dw}qDx�Dx�Dy�Dy��Dz�Dz� D{D{��D|�D|� D|�qD}}qD~�D~��D�D� D�  D�>�D�� D�D�HD�B�D���D��HD�  D�@ D�� D���D��qD�>�D�~�D�� D�  D�@ D�� D��HD�HD�B�D���D�� D���D�@ D�� D��qD�HD�@ D�� D�� D��qD�@ D�~�D��qD�HD�AHD�� D��qD���D�AHD��HD�� D���D�>�D�� D�� D�  D�@ D�� D���D��D�@ D�� D�� D���D�>�D�~�D�� D�  D�@ D��HD�D�  D�@ D���D��HD�  D�>�D�}qD���D�HD�@ D�}qD���D�HD�@ D�~�D�� D�HD�@ D�� D��HD�  D�>�D�~�D���D�  D�>�D�~�D��HD�HD�@ D�� D���D�HD�AHD�� D��HD�  D�@ D��HD��HD���D�>�D��HD�� D���D�>�D�� D�� D�  D�>�D�� D���D�  D�AHD�� D�� D�  D�>�D�~�D���D���D�@ D�� D�� D�  D�AHD��HD�� D�  D�@ D�� D�� D�  D�@ D�~�D���D��D�B�D��HD�� D���D�@ D��HD���D�  D�@ D�~�D�� D���D�>�D�� D�� D���D�@ D�� D���D���D�@ D�� D��qD���D�>�D�� D�� D���D�@ D�� D���D�  D�>�D�~�D�� D���D�=qD�� D��HD���D�=qD�~�D��HD�  D�@ D��HD��HD�  D�@ D��HD�D�  D�>�D�� D��HD�HD�>�D�� D�� D���D�>�D�}qD���D�  D�@ D�� D���D��qD�=qD�}qD���D���D�@ D�� D�� D�HD�AHD��HD��HD�  D�@ D�� D�� D�  D�>�D�� D��HD�HD�AHD�� D�� D�  D�@ D�~�D���D���D�@ D��HD�� D���D�@ D��HD��HD�  D�>�D�}qD���D�  D�@ D�~�D��qD���D�AHD��HD�� D�  D�@ D�~�D���D���D�>�D�~�D��qD���D�@ DHD��HD�HD�AHDÀ DýqD���D�AHDāHD�� D�  D�AHDŁHD��HD���D�>�D�~�D�� D�  D�>�Dǀ D��HD�HD�AHDȁHD��HD�HD�B�DɁHDɾ�D�  D�@ D�~�Dʾ�D�  D�@ D�~�D˾�D�HD�@ D̀ D�� D�  D�>�D�~�D�� D�HD�@ D΀ Dξ�D�  D�@ D�~�D�� D�  D�>�D�~�D�� D���D�>�D�~�D�� D���D�>�DҀ D��HD�  D�@ DӀ DӾ�D�  D�>�D�~�D��HD�HD�AHD�~�Dվ�D���D�@ DցHD�� D�  D�@ D׀ D׽qD���D�AHD�~�Dؾ�D�  D�>�D�~�D�� D�  D�@ Dڀ D��HD�HD�AHDہHD�� D�  D�@ D܀ D��HD�HD�AHD�~�D�� D���D�=qDށHD�D��D�AHD߂�D��HD���D�>�D�~�DྸD���D�@ D� D��HD���D�>�D� D⾸D�  D�B�D�HD��HD�HD�B�D� D�� D�HD�AHD�HD�� D�  D�@ D�HD澸D���D�@ D� D�� D�HD�>�D�~�D�� D���D�=qD�}qD�� D�  D�AHDꂏD��HD���D�>�D� D�� D�HD�>�D�}qD�qD�  D�B�D킏D�� D���D�>�D�~�DD�  D�@ D� D��HD�  D�@ D��HD�� D�  D�>�D�~�D�� D�  D�>�D�~�D�D��qD�>�D� D��HD���D�@ D�~�D��qD���D�>�D�~�D�� D�HD�@ D�� D�� D���D�>�D�~�D�� D�  D�>�D�� D��HD�HD�AHD��HD�� D���D�>�D��HD��HD�  ?#�
?.{?8Q�?�=q?��
?���?�
=?�ff@�\@\)@!G�@(��@=p�@L��@Tz�@h��@xQ�@�G�@���@��@�Q�@�(�@��@�{@�@���@\@�=q@�\)@�
=@�G�@�ff@�\)@�Q�@��RA�
A��A(�A\)AA=qAp�A"�\A'�A*�HA0  A5A8Q�A>{AC33AEAI��AP  AS33AW
=A[�A`��Adz�Ag�Amp�Ar�\Aw
=Az=qA\)A�=qA�(�A�{A���A�33A���A�
=A��A�33A�A���A��\A�z�A�ffA���A��A��A��A�=qA�(�A�{A���A��A���A�
=A���A��
A�A��A\A�z�AƸRAȣ�A˅A�p�A�
=Aљ�A�z�A�ffA�  Aڏ\A��A޸RA��A�A���A�RA�G�A�A��A�RA�G�A�33A�z�A�ffA���A�=qA��A��B   B ��Bp�B=qB�B��Bp�B�RB  B	�B
{B
=Bz�BBffB�B��B{B�HB(�B��B�\B\)B��B{B�RB�
BG�BffB\)B Q�B!B"�HB#�B$��B&=qB'�B(Q�B)G�B*�HB,  B,��B.{B/\)B0Q�B1�B2ffB3�B4��B5��B6�HB8  B8��B9�B;33B;�
B<��B=�B?33B@(�B@��BABB�HBD  BD��BE��BF�RBH(�BIG�BJ=qBK\)BLz�BMBO
=BP  BP��BR�\BS�BTQ�BU��BW
=BW�
BX��BZ{B[33B\  B\��B]�B_33B`z�Bap�Bb=qBc�Bd��Be�Bf�RBg�
Bi�BjffBk\)Bl(�Bmp�Bn�RBp  Bp��BqBs33Btz�Bup�BvffBw�Bx��Bz{Bz�HB|  B}G�B~�RB�
B�z�B��HB���B�=qB��RB�33B�B�ffB�
=B��B��B�z�B��B�B�(�B���B�33B�B�ffB��HB�G�B�B�z�B�
=B�p�B��B��\B�33B���B�  B��\B�G�B��
B�(�B���B�33B��
B�ffB���B�G�B��B��\B�
=B�\)B��B�z�B��B���B�  B���B�33B���B�{B���B�G�B�B�(�B��RB�\)B�  B�z�B��HB�p�B�{B���B��B���B�(�B���B�\)B�B�(�B��HB�p�B��B�ffB��HB��B�=qB���B�G�B�B�ffB�
=B��B�(�B���B��B��
B�z�B��B���B�{B��RB�\)B�{B��\B�
=B�B�ffB�
=B��B�  B��RB�\)B�  B���B�
=B��B�ffB��B��B�(�B���B�p�B�(�B��HB�p�B��B\B�33B�  Bģ�B�G�B�B�Q�B�
=B�B�ffB��HB�p�B�{B��HB˅B�(�B̸RB�33B��BΣ�B�G�B��
B�Q�B��HBљ�B�Q�B�
=B�p�B�{Bԣ�B�\)B�{B���B�\)B��B�z�B�
=Bٙ�B�=qB�
=BۮB�=qBܸRB�G�B��Bޏ\B�33B�B�(�B�RB�G�B�B�z�B�33B�B�Q�B���B�G�B�  B��B�33B�B�=qB���B�B�=qB�RB�\)B�  B�RB�G�B�B�Q�B�
=B��
B�z�B��HB�p�B�{B���B�p�B��
B�\B�\)B��B�z�B�
=B��B�ffB��B��B�{B���B��B�{B��\B��B��
B�z�B���B��C (�C z�C �RC  CQ�C�C
=CG�C�\C�HCG�C�\CC�C�CC  CffC�RC��C=qC��C�C33Cp�C�
C33Cp�C�C	  C	\)C	�RC	��C
33C
�C
�CG�Cz�C��C�C�CC
=CffC��C  CG�C��C  CG�C�C�HC=qC�CC{CffC��C(�Cp�C�RC  C\)C�RC
=CQ�C��C�CG�C��C�HC33C�C�HC=qC�\C��C(�C�C�C(�Cp�CC(�Cz�C�
C�C\)C�RC�Cp�C�RC  CQ�C�RC{C\)C�C��CG�C��C   C \)C ��C �C!=qC!��C"  C"Q�C"��C"�HC#(�C#�\C#�C$=qC$�C$��C%{C%p�C%��C&(�C&z�C&��C'{C'\)C'�C(  C(Q�C(�C)  C)Q�C)��C)�HC*(�C*�\C*�HC+33C+z�C+�RC,
=C,\)C,�RC-{C-ffC-�C-��C.=qC.�\C.�C/=qC/��C/�HC0(�C0p�C0�RC1  C1Q�C1��C1��C2G�C2��C2�C3�C3p�C3�RC4{C4ffC4C5
=C5\)C5��C5�HC6(�C6z�C6C7�C7p�C7�RC7��C8=qC8�C8�HC9(�C9�C9�
C:{C:\)C:��C:�C;G�C;��C;�
C<{C<ffC<�RC=
=C=\)C=��C=�HC>�C>ffC>�RC?  C?ffC?�C@  C@=qC@z�C@��CA{CAp�CACB  CBG�CB�CB�
CC(�CC�CC��CD�CDffCD�CD�CE33CEz�CE��CF(�CFz�CF��CG{CG\)CG��G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111                                                                                                                                                                                                                                                                                                                             ?�  ?��H@@  @�  @��R@�G�@�G�A   A{A\)A,��AAG�A`  A�Q�A��A�  A�  A�  A�Q�A�Q�A�Q�A��B  B  B(�B (�B((�B0  B8(�B@(�BH  BPQ�BX(�B_�Bh  Bp  Bx  B�  B�{B�  B�  B�(�B�  B�(�B�(�B�  B�{B�  B�  B�  B��B��
B��B�{B�{B�  B�{B�{B�  B�  B�(�B�{B�{B�{B�  B�  B�  B�  B�  C 
=C
=C  C  C
=C

=C  C��C��C{C{C  C��C�HC  C�C {C"
=C$
=C&
=C'�C)�C+�C-��C0  C1��C3�C5��C8  C:  C<  C>
=C@{CB  CD  CF  CG��CJ
=CL  CM��CO��CR
=CT  CV  CW��CY��C[��C]��C`  Cb
=Cd  Cf
=Ch  Ci��Ck�Cn  Cp  Cq��Ct
=Cv  Cx  Cz  C|  C~
=C�  C�  C�C�C�C���C�  C�
=C�C���C�  C���C���C�  C���C�  C�  C�C�  C�C�  C�  C�  C�  C�C�  C�  C�  C�  C�C�  C�  C���C�  C�  C�  C�  C�C�  C�C���C�  C�  C�C�C�C�  C�C�C���C���C�C�C�  C�C�  C��C���C�C���C�  C���C�C�  C�  C���C�C�C���C��C���C�C�C�C�  C�  C�
=C�C���C���C�  C�  C�C�
=C�C���C���C���C�C���C���C���C�C�C�C�
=C�
=C�C���C���C���C���C�  C���C���C���C�  C�C�
=C�C���C���C���C�  C�C�  C�C�
=C�C�C�C�C�C�C�  C���C���C��D   D � D �qD}qD��D}qD�qD� D�qD� D  D��D�D� D�qD}qD  D� D	  D	� D
�D
��D
��DxRD��D}qD  D}qD  D}qD�qD}qD��D� D�D��D�qD� D  D� D�D}qD  D� D�D��D  D��D�D� D�qD��DD��DD�D�D� D  D}qD�qD� D�qD� D   D }qD!  D!� D"  D"� D"�qD#� D$�D$� D%  D%��D%�qD&}qD'  D'� D(  D(��D)  D)� D)�qD*� D+D+��D+�qD,z�D,��D-}qD.  D.� D/�D/��D0  D0}qD0�qD1}qD2  D2� D3  D3z�D3�qD4��D5  D5z�D6  D6� D6�qD7� D7�qD8� D9  D9}qD9�qD:}qD:�qD;� D<  D<��D=�D=z�D=�qD>}qD?  D?}qD?�qD@��DA  DA}qDB  DB� DB�qDC}qDD  DD� DE  DE� DF  DF� DF��DG� DHDH� DH�qDI� DJ  DJ� DK�DK��DL  DL� DM�DM}qDM�qDN� DO�DO��DP  DP� DQ  DQ��DR  DR}qDR�qDS}qDS�qDT}qDU  DU}qDU�qDV� DWDW��DW�qDX� DY  DY� DY�qDZ}qD[  D[}qD\  D\}qD]�D]�D^  D^}qD^��D_}qD`�D`��Da�Da� Db  Db��Dc  Dc��Dd  Dd}qDe  De� Df�Df� DgDg�Dh  Dh� Di�Di��Dj�Dj��Dk�Dk��DlDl� Dl�qDm��Dn  Dn}qDo�Do��Dp�Dp� Dq  Dq� Dr�Dr� Ds  Ds��Dt�Dt}qDu  Du��Dv  Dv}qDv��Dw}qDx�Dx�Dy�Dy��Dz�Dz� D{D{��D|�D|� D|�qD}}qD~�D~��D�D� D�  D�>�D�� D�D�HD�B�D���D��HD�  D�@ D�� D���D��qD�>�D�~�D�� D�  D�@ D�� D��HD�HD�B�D���D�� D���D�@ D�� D��qD�HD�@ D�� D�� D��qD�@ D�~�D��qD�HD�AHD�� D��qD���D�AHD��HD�� D���D�>�D�� D�� D�  D�@ D�� D���D��D�@ D�� D�� D���D�>�D�~�D�� D�  D�@ D��HD�D�  D�@ D���D��HD�  D�>�D�}qD���D�HD�@ D�}qD���D�HD�@ D�~�D�� D�HD�@ D�� D��HD�  D�>�D�~�D���D�  D�>�D�~�D��HD�HD�@ D�� D���D�HD�AHD�� D��HD�  D�@ D��HD��HD���D�>�D��HD�� D���D�>�D�� D�� D�  D�>�D�� D���D�  D�AHD�� D�� D�  D�>�D�~�D���D���D�@ D�� D�� D�  D�AHD��HD�� D�  D�@ D�� D�� D�  D�@ D�~�D���D��D�B�D��HD�� D���D�@ D��HD���D�  D�@ D�~�D�� D���D�>�D�� D�� D���D�@ D�� D���D���D�@ D�� D��qD���D�>�D�� D�� D���D�@ D�� D���D�  D�>�D�~�D�� D���D�=qD�� D��HD���D�=qD�~�D��HD�  D�@ D��HD��HD�  D�@ D��HD�D�  D�>�D�� D��HD�HD�>�D�� D�� D���D�>�D�}qD���D�  D�@ D�� D���D��qD�=qD�}qD���D���D�@ D�� D�� D�HD�AHD��HD��HD�  D�@ D�� D�� D�  D�>�D�� D��HD�HD�AHD�� D�� D�  D�@ D�~�D���D���D�@ D��HD�� D���D�@ D��HD��HD�  D�>�D�}qD���D�  D�@ D�~�D��qD���D�AHD��HD�� D�  D�@ D�~�D���D���D�>�D�~�D��qD���D�@ DHD��HD�HD�AHDÀ DýqD���D�AHDāHD�� D�  D�AHDŁHD��HD���D�>�D�~�D�� D�  D�>�Dǀ D��HD�HD�AHDȁHD��HD�HD�B�DɁHDɾ�D�  D�@ D�~�Dʾ�D�  D�@ D�~�D˾�D�HD�@ D̀ D�� D�  D�>�D�~�D�� D�HD�@ D΀ Dξ�D�  D�@ D�~�D�� D�  D�>�D�~�D�� D���D�>�D�~�D�� D���D�>�DҀ D��HD�  D�@ DӀ DӾ�D�  D�>�D�~�D��HD�HD�AHD�~�Dվ�D���D�@ DցHD�� D�  D�@ D׀ D׽qD���D�AHD�~�Dؾ�D�  D�>�D�~�D�� D�  D�@ Dڀ D��HD�HD�AHDہHD�� D�  D�@ D܀ D��HD�HD�AHD�~�D�� D���D�=qDށHD�D��D�AHD߂�D��HD���D�>�D�~�DྸD���D�@ D� D��HD���D�>�D� D⾸D�  D�B�D�HD��HD�HD�B�D� D�� D�HD�AHD�HD�� D�  D�@ D�HD澸D���D�@ D� D�� D�HD�>�D�~�D�� D���D�=qD�}qD�� D�  D�AHDꂏD��HD���D�>�D� D�� D�HD�>�D�}qD�qD�  D�B�D킏D�� D���D�>�D�~�DD�  D�@ D� D��HD�  D�@ D��HD�� D�  D�>�D�~�D�� D�  D�>�D�~�D�D��qD�>�D� D��HD���D�@ D�~�D��qD���D�>�D�~�D�� D�HD�@ D�� D�� D���D�>�D�~�D�� D�  D�>�D�� D��HD�HD�AHD��HD�� D���D�>�D��HD��HG�O�?#�
?.{?8Q�?�=q?��
?���?�
=?�ff@�\@\)@!G�@(��@=p�@L��@Tz�@h��@xQ�@�G�@���@��@�Q�@�(�@��@�{@�@���@\@�=q@�\)@�
=@�G�@�ff@�\)@�Q�@��RA�
A��A(�A\)AA=qAp�A"�\A'�A*�HA0  A5A8Q�A>{AC33AEAI��AP  AS33AW
=A[�A`��Adz�Ag�Amp�Ar�\Aw
=Az=qA\)A�=qA�(�A�{A���A�33A���A�
=A��A�33A�A���A��\A�z�A�ffA���A��A��A��A�=qA�(�A�{A���A��A���A�
=A���A��
A�A��A\A�z�AƸRAȣ�A˅A�p�A�
=Aљ�A�z�A�ffA�  Aڏ\A��A޸RA��A�A���A�RA�G�A�A��A�RA�G�A�33A�z�A�ffA���A�=qA��A��B   B ��Bp�B=qB�B��Bp�B�RB  B	�B
{B
=Bz�BBffB�B��B{B�HB(�B��B�\B\)B��B{B�RB�
BG�BffB\)B Q�B!B"�HB#�B$��B&=qB'�B(Q�B)G�B*�HB,  B,��B.{B/\)B0Q�B1�B2ffB3�B4��B5��B6�HB8  B8��B9�B;33B;�
B<��B=�B?33B@(�B@��BABB�HBD  BD��BE��BF�RBH(�BIG�BJ=qBK\)BLz�BMBO
=BP  BP��BR�\BS�BTQ�BU��BW
=BW�
BX��BZ{B[33B\  B\��B]�B_33B`z�Bap�Bb=qBc�Bd��Be�Bf�RBg�
Bi�BjffBk\)Bl(�Bmp�Bn�RBp  Bp��BqBs33Btz�Bup�BvffBw�Bx��Bz{Bz�HB|  B}G�B~�RB�
B�z�B��HB���B�=qB��RB�33B�B�ffB�
=B��B��B�z�B��B�B�(�B���B�33B�B�ffB��HB�G�B�B�z�B�
=B�p�B��B��\B�33B���B�  B��\B�G�B��
B�(�B���B�33B��
B�ffB���B�G�B��B��\B�
=B�\)B��B�z�B��B���B�  B���B�33B���B�{B���B�G�B�B�(�B��RB�\)B�  B�z�B��HB�p�B�{B���B��B���B�(�B���B�\)B�B�(�B��HB�p�B��B�ffB��HB��B�=qB���B�G�B�B�ffB�
=B��B�(�B���B��B��
B�z�B��B���B�{B��RB�\)B�{B��\B�
=B�B�ffB�
=B��B�  B��RB�\)B�  B���B�
=B��B�ffB��B��B�(�B���B�p�B�(�B��HB�p�B��B\B�33B�  Bģ�B�G�B�B�Q�B�
=B�B�ffB��HB�p�B�{B��HB˅B�(�B̸RB�33B��BΣ�B�G�B��
B�Q�B��HBљ�B�Q�B�
=B�p�B�{Bԣ�B�\)B�{B���B�\)B��B�z�B�
=Bٙ�B�=qB�
=BۮB�=qBܸRB�G�B��Bޏ\B�33B�B�(�B�RB�G�B�B�z�B�33B�B�Q�B���B�G�B�  B��B�33B�B�=qB���B�B�=qB�RB�\)B�  B�RB�G�B�B�Q�B�
=B��
B�z�B��HB�p�B�{B���B�p�B��
B�\B�\)B��B�z�B�
=B��B�ffB��B��B�{B���B��B�{B��\B��B��
B�z�B���B��C (�C z�C �RC  CQ�C�C
=CG�C�\C�HCG�C�\CC�C�CC  CffC�RC��C=qC��C�C33Cp�C�
C33Cp�C�C	  C	\)C	�RC	��C
33C
�C
�CG�Cz�C��C�C�CC
=CffC��C  CG�C��C  CG�C�C�HC=qC�CC{CffC��C(�Cp�C�RC  C\)C�RC
=CQ�C��C�CG�C��C�HC33C�C�HC=qC�\C��C(�C�C�C(�Cp�CC(�Cz�C�
C�C\)C�RC�Cp�C�RC  CQ�C�RC{C\)C�C��CG�C��C   C \)C ��C �C!=qC!��C"  C"Q�C"��C"�HC#(�C#�\C#�C$=qC$�C$��C%{C%p�C%��C&(�C&z�C&��C'{C'\)C'�C(  C(Q�C(�C)  C)Q�C)��C)�HC*(�C*�\C*�HC+33C+z�C+�RC,
=C,\)C,�RC-{C-ffC-�C-��C.=qC.�\C.�C/=qC/��C/�HC0(�C0p�C0�RC1  C1Q�C1��C1��C2G�C2��C2�C3�C3p�C3�RC4{C4ffC4C5
=C5\)C5��C5�HC6(�C6z�C6C7�C7p�C7�RC7��C8=qC8�C8�HC9(�C9�C9�
C:{C:\)C:��C:�C;G�C;��C;�
C<{C<ffC<�RC=
=C=\)C=��C=�HC>�C>ffC>�RC?  C?ffC?�C@  C@=qC@z�C@��CA{CAp�CACB  CBG�CB�CB�
CC(�CC�CC��CD�CDffCD�CD�CE33CEz�CE��CF(�CFz�CF��CG{CG\)CG��G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111                                                                                                                                                                                                                                                                                                                             @�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@��@��@ȞG�O�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�A�VA�oA��A��A��A� �A�"�A�$�A�+A�-A�+A�+A�(�A�1'A�/A�33A�33A�1'A�1'A�1'A�33A�33A�5?A�?}A�;dA�9XA�E�A�E�A�K�A�M�A�K�A�XA�VA�M�A��A���A��A҅A��AѺ^A�v�A�C�A�v�A�;dA��A�-A�dZA�-A�bNA�
=AȋDAǴ9Aŗ�AÅA�A�VA���A�M�A�hsA���A�v�A�x�A���A�^5A���A�;dA���A�S�A�S�A�dZA�"�A�dZA��mA���A�1'A��^A�t�A���A��A�VA�bNA�t�A�;dA�G�A�~�A�"�A���A�$�A��^A��yA�  A��9A���A���A���A�O�A���A��jA�ffA�~�A��TA�A}�FAx�Aw+Au7LAsAs�hAq+Ao�An�HAm�FAl1Ai�wAf�HAc�
Ac+Ab�DAb9XA`�9A\bNAX��AV��AP�/AN�HAN~�ANE�AM��ALz�AJ��AJA�AI��AH��AGK�AD�jAB{AAVA>A�A=&�A<��A:��A8ĜA7"�A6�A5�-A2��A0�A//A-t�A-+A+�wA(�RA'��A&�A&�!A&=qA%C�A$�\A#K�A!oA 5?AhsA�#A�A�^A�7Al�AAt�A�\AG�A��A{A��AM�A?}A�-A��A�A
��A	�wAz�A�hA1'AI�A��A/A ��A ȴA �A ZA �@��P@�-@�(�@���@��#@�D@�"�@�K�@�o@�ȴ@�5?@�!@��@�"�@�R@�n�@�$�@�@�O�@��@�+@�$�@�O�@�P@�~�@��@�h@��@��@ާ�@ޟ�@ޟ�@ޏ\@��@�G�@�j@۾w@۝�@�K�@�+@��@�-@���@�O�@�V@ؓu@��@�t�@�33@���@�5?@�@�`B@�V@��`@�Z@�  @Ӆ@ӶF@�1'@��;@�K�@��H@ҸR@�$�@�`B@��/@�Ĝ@д9@�I�@ϥ�@�;d@��H@���@�n�@�ff@��T@�G�@̴9@�9X@˝�@�33@�"�@��y@�n�@�X@� �@�t�@�"�@Ƨ�@���@ă@�I�@��@��m@Å@��@�@��7@�p�@��`@�A�@���@�S�@�"�@��@�^5@�x�@���@�1@��P@�C�@�t�@�"�@��!@���@�M�@�@���@��@�X@�O�@�G�@��@�V@���@��`@���@��9@���@�(�@���@��@���@�@��T@���@��@�x�@�O�@�/@��`@��9@�Q�@�dZ@��!@��T@��7@�x�@�hs@�`B@�`B@�?}@�/@�V@�Z@���@�dZ@�C�@�o@��@��\@�V@�E�@�=q@�5?@��#@��@�r�@��@�
=@��R@�~�@�v�@�n�@�M�@�$�@�p�@�Ĝ@�Q�@���@�S�@�K�@��y@���@���@��^@��7@�hs@�G�@��`@��9@��u@�9X@���@�|�@�S�@�;d@�"�@�o@�o@�o@�
=@��H@�n�@�V@�5?@�{@��-@��7@�hs@�?}@���@��j@��D@�r�@�j@�Z@�I�@� �@��@���@�"�@�=q@��^@���@�X@��@��`@�Ĝ@�z�@�I�@�9X@�  @��@�\)@�"�@��H@��!@���@��+@��+@�~�@�n�@�^5@�E�@��#@�@���@��7@�O�@��@�Ĝ@�bN@��m@�|�@�"�@�@��H@���@�v�@�=q@��#@�x�@�%@���@��@��`@��/@���@��j@��@�j@��@��@��@�33@�o@���@���@��!@��+@�=q@�@���@���@�x�@��j@��D@��@��@��@�z�@�z�@�r�@�9X@�  @�l�@��H@��+@�M�@�5?@��@��#@��#@���@��-@�hs@�?}@��@���@�1'@���@��@�K�@�;d@�o@��R@�v�@���@��@��u@�j@�b@��m@��w@���@�\)@�"�@��H@���@�ff@�V@�M�@�E�@�=q@�-@�J@���@�7L@��`@��9@�1@�l�@�C�@�;d@���@��@���@���@��@�X@�?}@�&�@�V@��@��@���@��D@�z�@�Z@�1@~��@~{@}V@|��@|1@{t�@{@z~�@z�@zJ@y�#@y�@w�;@w��@w|�@v��@vȴ@v�R@v��@v��@v�+@vv�@u�T@u�@u`B@u�@t�/@s��@sC�@s"�@r�H@r�!@q��@pr�@o�@o�@ol�@o\)@o;d@o
=@n�R@nv�@nV@n$�@m�T@m`B@m�@l��@l��@l��@lI�@k��@j�\@jM�@jJ@i�#@i��@ix�@iX@h��@h�@hA�@h  @g�;@g��@g�@gl�@g;d@g
=@f�y@f��@fV@e�@e@e`B@d��@d�@c��@cƨ@ct�@c@c@c@c@b�@b��@b�!@b�@a�7@aX@`��@`��@`�u@`A�@_�;@_�P@_|�@^�y@^��@]�-@]?}@]?}@]?}@]?}@]/@]/@]/@\��@\��@\�j@\j@[�
@[o@Z��@ZJ@Y��@YX@X��@X�9@X�@XQ�@W�@W��@W\)@WK�@V�@V$�@U�-@T��@SS�@R��@Rn�@R-@R�@RJ@Q�@Qhs@Q�@P��@P�u@Pr�@PA�@P  @O�@O|�@N�R@N{@M�-@M?}@L�j@L��@L9X@L1@Kƨ@K"�@JM�@Ix�@IX@I&�@H�`@H�9@HQ�@G��@F��@F�@Fȴ@F�+@F@E@Ep�@EV@D�j@Dz�@DZ@DI�@C��@C"�@B^5@BJ@A��@A�7@AG�@@Ĝ@@�u@@A�@@  @?�w@?��@?�P@?|�@?K�@>ff@>@>@>@>@=�@=�-@=�@=`B@=O�@=/@<�/@<�@<j@;ƨ@:��@:M�@:-@9�#@9��@9�7@9�7@9�7@9hs@97L@9&�@9�@9%@8�`@8�9@8bN@8b@7�P@7;d@6�y@6�y@7
=@6$�@6{@6$�@6$�@6{@5��@4�@4I�@49X@4�@41@3o@1��@1%@0��@0��@0�@0r�@0bN@0 �@/|�@.�@.��@.ff@.5?@.@-�@-��@-�h@-�@,��@+��@+��@+S�@+"�@*�H@*��@*��@*�!@*�\@*^5@*M�@*-@)��@)�@)��@)�^@)G�@(Q�@( �@'�@'��@'l�@'+@'+@'+@'+@'�@'
=@'
=@'
=@&�@&$�@%?}@$��@$�D@$z�@#��@"��@"�!@"�!@"��@"��@"~�@"n�@"M�@"=q@"=q@!�@!�7@!�@ �u@ 1'@�;@�w@�@|�@;d@
=@�@��@E�@�@�T@�T@��@�@O�@�@�/@��@I�@��@�F@��@S�@C�@C�@o@~�@M�@-@�@�@�@J@J@�@�#@�^@�^@x�@&�@��@�9@r�@1'@b@�@;d@ȴ@v�@V@V@E�@$�@��@�-@�h@p�@`B@O�@?}@/@��@�/@�j@�j@��@�@�m@�F@��@��@��@��@��@�@C�@@�H@��@��@n�@�@��@��@x�@hs@&�@%@��@�9@�9@�9@�u@r�@1'@b@�;@��@�P@K�@+@�@�R@�+@V@E�@E�@$�@@��@��@p�@O�@/@�/@�j@�@�D@z�@j@(�@�@1@�m@ƨ@ƨ@�@dZ@dZ@S�@@
�H@
��@
��@
n�@
=q@
�@
J@	��@	�#@	�#@	�#@	��@	��@	��A���A���A�oA�{A�oA�bA��A�JA�VA�{A��A�{A��A��A��A��A�$�A��A��A�"�A��A��A��A� �A��A��A� �A�"�A��A� �A�&�A� �A�"�A�"�A� �A�"�A�-A�+A�&�A�+A�-A�(�A�-A�/A�+A�-A�-A�(�A�+A�/A�(�A�(�A�+A�+A�+A�+A�+A�+A�&�A�(�A�-A�/A�-A�1'A�5?A�33A�1'A�1'A�/A�$�A�+A�5?A�+A�33A�5?A�5?A�33A�1'A�7LA�5?A�1'A�5?A�7LA�5?A�1'A�5?A�7LA�1'A�/A�1'A�33A�1'A�/A�33A�33A�1'A�/A�33A�33A�/A�33A�1'A�/A�+A�/A�1'A�1'A�1'A�5?A�1'A�1'A�5?A�7LA�33A�1'A�7LA�33A�1'A�33A�5?A�5?A�1'A�1'A�5?A�5?A�33A�1'A�5?A�5?A�1'A�5?A�5?A�5?A�1'A�5?A�9XA�?}A�7LA�;dA�G�A�E�A�?}A�C�A�?}A�?}A�7LA�7LA�9XA�7LA�5?A�9XA�9XA�5?A�33A�5?A�?}A�E�A�C�A�E�A�C�A�G�A�?}A�E�A�E�A�A�A�C�A�G�A�I�A�E�A�G�A�K�A�I�A�G�A�K�A�M�A�M�A�I�A�K�A�M�A�I�A�O�A�S�A�XA�I�A�K�A�M�A�K�A�G�A�E�A�E�A�I�A�O�A�XA�VA�XA�\)A�\)A�XA�S�A�ZA�S�A�S�A�S�A�VA�VA�O�A�VA�XA�VA�Q�A�M�A�O�A�Q�A�M�A�I�A�;dA�;dA�1'A�  A���A��mA�AՕ�A�v�A��AԼjA���AԼjAԲ-Aԗ�A�l�A�XA�5?A�(�A���AӅA�r�A�XA�  A�ĜAҁA�1'A�JA�VA�bA�1A���A��A��A��yA��A���A���A�ƨA�ĜAѾwAѲ-Aѥ�Aѡ�Aѝ�Aї�A�~�A�n�A�dZA�bNA�^5A�\)A�^5A�\)A�XA�Q�A�Q�A�A�A���A���Aа!AЉ7A�hsA�^5A�Q�A�M�A�M�A�K�A�I�A�C�A�?}A�;dA�7LA�(�A��A��HAϋDA�33A�ȴAΕ�A�v�A�=qAͰ!A͝�A�z�A�I�A��A�Ȁ\Ả7A�|�A�t�A�dZA�dZA�dZA�VA�=qA�{A��A˟�A�VA���Aʟ�Aʛ�Aʏ\Aʇ+A�hsA�bNA�S�A�G�A�?}A�33A�(�A� �A� �A��A�
=Aɰ!A�`BA���Aȉ7A�x�A�Q�A�=qA�/A�{A���A�ȴAǟ�A�x�A�XA�=qA�JA���A�G�Aĥ�A�r�A�XA�(�A���A���A�~�A�G�A�JA��A��yA���A¡�A�|�A�VA�=qA�$�A�"�A�"�A��A���A��HA���A��A���A��uA��DA��A��A�n�A�dZA�Q�A�G�A�?}A�(�A�1A��A��FA�dZA���A���A�r�A���A��A���A��RA���A��A�z�A�v�A�v�A�v�A�t�A�r�A�v�A�x�A�z�A�z�A�x�A�v�A�v�A�p�A�hsA�1'A��A�ƨA��DA�VA��A���A���A���A�?}A��A���A���A���A���A��PA�~�A�t�A�jA�`BA�VA�K�A�5?A� �A�JA�1A���A��TA��jA��A�I�A�JA���A���A�n�A�5?A��A��A��^A��+A�l�A�XA�-A�oA�bA���A��A���A�v�A�oA�ȴA��!A��uA�z�A�33A��A��FA���A��7A�v�A�l�A�^5A�M�A�9XA�/A�&�A�VA��A��
A��-A�l�A�M�A� �A��`A��!A���A�~�A�Q�A�S�A���A�Q�A��
A�5?A� �A�1A��A��RA���A��\A�~�A�x�A�|�A�z�A�t�A�n�A�jA�^5A�-A��A��#A���A��A��A���A���A�r�A�33A���A�1A���A���A��+A�t�A�^5A���A��A��FA���A�z�A�jA�/A��PA��TA��wA���A�Q�A�1A���A��/A�JA�+A���A�1A���A�p�A�;dA��mA�VA���A�&�A��A�Q�A��PA��A���A�"�A�|�A���A�M�A��A��/A��RA���A�S�A� �A�oA��#A��A�jA��A�  A�\)A���A�ZA�/A�1A���A��A���A���A�I�A��jA�9XA���A��
A���A�n�A�A�A��!A�bA�33A�JA�JA���A��A�S�A��
A�ffA�oA��#A��A�p�A�O�A��A�{A��mA��TA��/A��
A���A���A�A��RA��FA��!A���A���A���A���A���A���A��+A��A�p�A�E�A��A��A�ƨA���A�jA��PA��A��A��TA��;A��A��jA���A���A��\A�VA���A��\A���A��yA��#A�ĜA��RA��A���A��+A�z�A��A���A�;dA���A���A�hsA�7LA��wA�(�A��yA��A�PA7LA&�A�A~�A~�+A~1A}��A}S�A}�A|1'Az�`Ay�^Ay\)Ax�RAw�Awp�Aw`BAwC�Aw33Aw+Aw33Aw�Aw
=Aw%Av�`AvVG�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111                                                                                                                                                                                                                                                                                                                             A�VA�oA��A��A��A� �A�"�A�$�A�+A�-A�+A�+A�(�A�1'A�/A�33A�33A�1'A�1'A�1'A�33A�33A�5?A�?}A�;dA�9XA�E�A�E�A�K�A�M�A�K�A�XA�VA�M�A��A���A��A҅A��AѺ^A�v�A�C�A�v�A�;dA��A�-A�dZA�-A�bNA�
=AȋDAǴ9Aŗ�AÅA�A�VA���A�M�A�hsA���A�v�A�x�A���A�^5A���A�;dA���A�S�A�S�A�dZA�"�A�dZA��mA���A�1'A��^A�t�A���A��A�VA�bNA�t�A�;dA�G�A�~�A�"�A���A�$�A��^A��yA�  A��9A���A���A���A�O�A���A��jA�ffA�~�A��TA�A}�FAx�Aw+Au7LAsAs�hAq+Ao�An�HAm�FAl1Ai�wAf�HAc�
Ac+Ab�DAb9XA`�9A\bNAX��AV��AP�/AN�HAN~�ANE�AM��ALz�AJ��AJA�AI��AH��AGK�AD�jAB{AAVA>A�A=&�A<��A:��A8ĜA7"�A6�A5�-A2��A0�A//A-t�A-+A+�wA(�RA'��A&�A&�!A&=qA%C�A$�\A#K�A!oA 5?AhsA�#A�A�^A�7Al�AAt�A�\AG�A��A{A��AM�A?}A�-A��A�A
��A	�wAz�A�hA1'AI�A��A/A ��A ȴA �A ZA �@��P@�-@�(�@���@��#@�D@�"�@�K�@�o@�ȴ@�5?@�!@��@�"�@�R@�n�@�$�@�@�O�@��@�+@�$�@�O�@�P@�~�@��@�h@��@��@ާ�@ޟ�@ޟ�@ޏ\@��@�G�@�j@۾w@۝�@�K�@�+@��@�-@���@�O�@�V@ؓu@��@�t�@�33@���@�5?@�@�`B@�V@��`@�Z@�  @Ӆ@ӶF@�1'@��;@�K�@��H@ҸR@�$�@�`B@��/@�Ĝ@д9@�I�@ϥ�@�;d@��H@���@�n�@�ff@��T@�G�@̴9@�9X@˝�@�33@�"�@��y@�n�@�X@� �@�t�@�"�@Ƨ�@���@ă@�I�@��@��m@Å@��@�@��7@�p�@��`@�A�@���@�S�@�"�@��@�^5@�x�@���@�1@��P@�C�@�t�@�"�@��!@���@�M�@�@���@��@�X@�O�@�G�@��@�V@���@��`@���@��9@���@�(�@���@��@���@�@��T@���@��@�x�@�O�@�/@��`@��9@�Q�@�dZ@��!@��T@��7@�x�@�hs@�`B@�`B@�?}@�/@�V@�Z@���@�dZ@�C�@�o@��@��\@�V@�E�@�=q@�5?@��#@��@�r�@��@�
=@��R@�~�@�v�@�n�@�M�@�$�@�p�@�Ĝ@�Q�@���@�S�@�K�@��y@���@���@��^@��7@�hs@�G�@��`@��9@��u@�9X@���@�|�@�S�@�;d@�"�@�o@�o@�o@�
=@��H@�n�@�V@�5?@�{@��-@��7@�hs@�?}@���@��j@��D@�r�@�j@�Z@�I�@� �@��@���@�"�@�=q@��^@���@�X@��@��`@�Ĝ@�z�@�I�@�9X@�  @��@�\)@�"�@��H@��!@���@��+@��+@�~�@�n�@�^5@�E�@��#@�@���@��7@�O�@��@�Ĝ@�bN@��m@�|�@�"�@�@��H@���@�v�@�=q@��#@�x�@�%@���@��@��`@��/@���@��j@��@�j@��@��@��@�33@�o@���@���@��!@��+@�=q@�@���@���@�x�@��j@��D@��@��@��@�z�@�z�@�r�@�9X@�  @�l�@��H@��+@�M�@�5?@��@��#@��#@���@��-@�hs@�?}@��@���@�1'@���@��@�K�@�;d@�o@��R@�v�@���@��@��u@�j@�b@��m@��w@���@�\)@�"�@��H@���@�ff@�V@�M�@�E�@�=q@�-@�J@���@�7L@��`@��9@�1@�l�@�C�@�;d@���@��@���@���@��@�X@�?}@�&�@�V@��@��@���@��D@�z�@�Z@�1@~��@~{@}V@|��@|1@{t�@{@z~�@z�@zJ@y�#@y�@w�;@w��@w|�@v��@vȴ@v�R@v��@v��@v�+@vv�@u�T@u�@u`B@u�@t�/@s��@sC�@s"�@r�H@r�!@q��@pr�@o�@o�@ol�@o\)@o;d@o
=@n�R@nv�@nV@n$�@m�T@m`B@m�@l��@l��@l��@lI�@k��@j�\@jM�@jJ@i�#@i��@ix�@iX@h��@h�@hA�@h  @g�;@g��@g�@gl�@g;d@g
=@f�y@f��@fV@e�@e@e`B@d��@d�@c��@cƨ@ct�@c@c@c@c@b�@b��@b�!@b�@a�7@aX@`��@`��@`�u@`A�@_�;@_�P@_|�@^�y@^��@]�-@]?}@]?}@]?}@]?}@]/@]/@]/@\��@\��@\�j@\j@[�
@[o@Z��@ZJ@Y��@YX@X��@X�9@X�@XQ�@W�@W��@W\)@WK�@V�@V$�@U�-@T��@SS�@R��@Rn�@R-@R�@RJ@Q�@Qhs@Q�@P��@P�u@Pr�@PA�@P  @O�@O|�@N�R@N{@M�-@M?}@L�j@L��@L9X@L1@Kƨ@K"�@JM�@Ix�@IX@I&�@H�`@H�9@HQ�@G��@F��@F�@Fȴ@F�+@F@E@Ep�@EV@D�j@Dz�@DZ@DI�@C��@C"�@B^5@BJ@A��@A�7@AG�@@Ĝ@@�u@@A�@@  @?�w@?��@?�P@?|�@?K�@>ff@>@>@>@>@=�@=�-@=�@=`B@=O�@=/@<�/@<�@<j@;ƨ@:��@:M�@:-@9�#@9��@9�7@9�7@9�7@9hs@97L@9&�@9�@9%@8�`@8�9@8bN@8b@7�P@7;d@6�y@6�y@7
=@6$�@6{@6$�@6$�@6{@5��@4�@4I�@49X@4�@41@3o@1��@1%@0��@0��@0�@0r�@0bN@0 �@/|�@.�@.��@.ff@.5?@.@-�@-��@-�h@-�@,��@+��@+��@+S�@+"�@*�H@*��@*��@*�!@*�\@*^5@*M�@*-@)��@)�@)��@)�^@)G�@(Q�@( �@'�@'��@'l�@'+@'+@'+@'+@'�@'
=@'
=@'
=@&�@&$�@%?}@$��@$�D@$z�@#��@"��@"�!@"�!@"��@"��@"~�@"n�@"M�@"=q@"=q@!�@!�7@!�@ �u@ 1'@�;@�w@�@|�@;d@
=@�@��@E�@�@�T@�T@��@�@O�@�@�/@��@I�@��@�F@��@S�@C�@C�@o@~�@M�@-@�@�@�@J@J@�@�#@�^@�^@x�@&�@��@�9@r�@1'@b@�@;d@ȴ@v�@V@V@E�@$�@��@�-@�h@p�@`B@O�@?}@/@��@�/@�j@�j@��@�@�m@�F@��@��@��@��@��@�@C�@@�H@��@��@n�@�@��@��@x�@hs@&�@%@��@�9@�9@�9@�u@r�@1'@b@�;@��@�P@K�@+@�@�R@�+@V@E�@E�@$�@@��@��@p�@O�@/@�/@�j@�@�D@z�@j@(�@�@1@�m@ƨ@ƨ@�@dZ@dZ@S�@@
�H@
��@
��@
n�@
=q@
�@
J@	��@	�#@	�#@	�#@	��@	��G�O�A���A���A�oA�{A�oA�bA��A�JA�VA�{A��A�{A��A��A��A��A�$�A��A��A�"�A��A��A��A� �A��A��A� �A�"�A��A� �A�&�A� �A�"�A�"�A� �A�"�A�-A�+A�&�A�+A�-A�(�A�-A�/A�+A�-A�-A�(�A�+A�/A�(�A�(�A�+A�+A�+A�+A�+A�+A�&�A�(�A�-A�/A�-A�1'A�5?A�33A�1'A�1'A�/A�$�A�+A�5?A�+A�33A�5?A�5?A�33A�1'A�7LA�5?A�1'A�5?A�7LA�5?A�1'A�5?A�7LA�1'A�/A�1'A�33A�1'A�/A�33A�33A�1'A�/A�33A�33A�/A�33A�1'A�/A�+A�/A�1'A�1'A�1'A�5?A�1'A�1'A�5?A�7LA�33A�1'A�7LA�33A�1'A�33A�5?A�5?A�1'A�1'A�5?A�5?A�33A�1'A�5?A�5?A�1'A�5?A�5?A�5?A�1'A�5?A�9XA�?}A�7LA�;dA�G�A�E�A�?}A�C�A�?}A�?}A�7LA�7LA�9XA�7LA�5?A�9XA�9XA�5?A�33A�5?A�?}A�E�A�C�A�E�A�C�A�G�A�?}A�E�A�E�A�A�A�C�A�G�A�I�A�E�A�G�A�K�A�I�A�G�A�K�A�M�A�M�A�I�A�K�A�M�A�I�A�O�A�S�A�XA�I�A�K�A�M�A�K�A�G�A�E�A�E�A�I�A�O�A�XA�VA�XA�\)A�\)A�XA�S�A�ZA�S�A�S�A�S�A�VA�VA�O�A�VA�XA�VA�Q�A�M�A�O�A�Q�A�M�A�I�A�;dA�;dA�1'A�  A���A��mA�AՕ�A�v�A��AԼjA���AԼjAԲ-Aԗ�A�l�A�XA�5?A�(�A���AӅA�r�A�XA�  A�ĜAҁA�1'A�JA�VA�bA�1A���A��A��A��yA��A���A���A�ƨA�ĜAѾwAѲ-Aѥ�Aѡ�Aѝ�Aї�A�~�A�n�A�dZA�bNA�^5A�\)A�^5A�\)A�XA�Q�A�Q�A�A�A���A���Aа!AЉ7A�hsA�^5A�Q�A�M�A�M�A�K�A�I�A�C�A�?}A�;dA�7LA�(�A��A��HAϋDA�33A�ȴAΕ�A�v�A�=qAͰ!A͝�A�z�A�I�A��A�Ȁ\Ả7A�|�A�t�A�dZA�dZA�dZA�VA�=qA�{A��A˟�A�VA���Aʟ�Aʛ�Aʏ\Aʇ+A�hsA�bNA�S�A�G�A�?}A�33A�(�A� �A� �A��A�
=Aɰ!A�`BA���Aȉ7A�x�A�Q�A�=qA�/A�{A���A�ȴAǟ�A�x�A�XA�=qA�JA���A�G�Aĥ�A�r�A�XA�(�A���A���A�~�A�G�A�JA��A��yA���A¡�A�|�A�VA�=qA�$�A�"�A�"�A��A���A��HA���A��A���A��uA��DA��A��A�n�A�dZA�Q�A�G�A�?}A�(�A�1A��A��FA�dZA���A���A�r�A���A��A���A��RA���A��A�z�A�v�A�v�A�v�A�t�A�r�A�v�A�x�A�z�A�z�A�x�A�v�A�v�A�p�A�hsA�1'A��A�ƨA��DA�VA��A���A���A���A�?}A��A���A���A���A���A��PA�~�A�t�A�jA�`BA�VA�K�A�5?A� �A�JA�1A���A��TA��jA��A�I�A�JA���A���A�n�A�5?A��A��A��^A��+A�l�A�XA�-A�oA�bA���A��A���A�v�A�oA�ȴA��!A��uA�z�A�33A��A��FA���A��7A�v�A�l�A�^5A�M�A�9XA�/A�&�A�VA��A��
A��-A�l�A�M�A� �A��`A��!A���A�~�A�Q�A�S�A���A�Q�A��
A�5?A� �A�1A��A��RA���A��\A�~�A�x�A�|�A�z�A�t�A�n�A�jA�^5A�-A��A��#A���A��A��A���A���A�r�A�33A���A�1A���A���A��+A�t�A�^5A���A��A��FA���A�z�A�jA�/A��PA��TA��wA���A�Q�A�1A���A��/A�JA�+A���A�1A���A�p�A�;dA��mA�VA���A�&�A��A�Q�A��PA��A���A�"�A�|�A���A�M�A��A��/A��RA���A�S�A� �A�oA��#A��A�jA��A�  A�\)A���A�ZA�/A�1A���A��A���A���A�I�A��jA�9XA���A��
A���A�n�A�A�A��!A�bA�33A�JA�JA���A��A�S�A��
A�ffA�oA��#A��A�p�A�O�A��A�{A��mA��TA��/A��
A���A���A�A��RA��FA��!A���A���A���A���A���A���A��+A��A�p�A�E�A��A��A�ƨA���A�jA��PA��A��A��TA��;A��A��jA���A���A��\A�VA���A��\A���A��yA��#A�ĜA��RA��A���A��+A�z�A��A���A�;dA���A���A�hsA�7LA��wA�(�A��yA��A�PA7LA&�A�A~�A~�+A~1A}��A}S�A}�A|1'Az�`Ay�^Ay\)Ax�RAw�Awp�Aw`BAwC�Aw33Aw+Aw33Aw�Aw
=Aw%Av�`AvVG�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111                                                                                                                                                                                                                                                                                                                             ;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��G�O�;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�B
P�B
P�B
P�B
PB
P�B
P}B
P}B
PB
PB
O�B
P}B
PHB
PHB
P}B
P}B
P�B
P�B
P}B
P}B
P}B
P�B
P�B
P}B
R B
Q�B
Q�B
R�B
S�B
U2B
U2B
S�B
X�B
YKB
[#B
c�B
X�B
FB
6�B
9$B
GEB
O�B
S�B
v+B
}VB
��B
�?B
�iB9$Bt�B�~B��B��B��B#�B1[B7�B;dB<�BJ#B_�Bl�BqB�PB��B�dB�<B�gB�jB��B�BB�?B�B�B��B�Bv�Bl�Bh>BZQBC�B;�B*�B$@B �B�;B��B��B�wB��B��BwfB[�BF?B/OB+6B+6B�B
�cB
��B
�&B
�B
�RB
�MB
�B
j�B
i�B
V�B
S�B
OBB
A B
:�B
33B
*�B
B
�B	�DB	��B	�B	��B	�B	��B	��B	��B	��B	�oB	��B	��B	�DB	��B	�oB	y>B	v`B	q�B	m]B	a�B	VmB	Q�B	C-B	?�B	:�B	9�B	/B	+�B	'�B	%FB	#�B	eB	�B	"B	B	
�B	�B	�B	uB	�B	AB��B��B�JB�rB��B�|B�B�B�>B��B�
B�B�B�B�;B�BچBޞB�B�2B�B�B��B�rB��B��B�2B	  B��B�8B�>B�	B�B�	B�fB�fB�fB��B�lB��B	�B	�B	�B	xB	�B	�B	:B	MB	B	uB	�B	 �B�cB�]B��B�PB	 4B	uB	�B	PB	�B	�B	!�B	%zB	0�B	8RB	8RB	7�B	8B	=B	>B	C�B	EmB	EmB	F?B	F?B	HB	MB	OvB	Q�B	S[B	W
B	^B	`�B	d�B	gmB	h�B	i�B	l�B	o5B	o5B	p;B	u%B	v`B	w�B	}VB	�iB	��B	��B	�GB	��B	��B	��B	��B	��B	�hB	��B	��B	�FB	��B	�+B	�1B	��B	��B	��B	��B	��B	��B	��B	�\B	�bB	�tB	��B	�LB	�0B	�kB	��B	��B	�LB	��B	�B	��B	��B	ÖB	�3B	��B	�B	��B	��B	�jB	��B	�pB	�}B	�
B	�WB	��B	�vB	�B	�,B	�B	�B	�B	� B	�B	�MB	�B	�ZB	�ZB	��B	��B	��B	��B	�fB	��B	��B	�lB	�B	�B	�B
 4B
B
�B
�B
�B
MB
�B
B
�B
�B
+B
xB
VB
�B
�B
 B
 B
 B
4B
hB
hB
�B
FB
�B
B
B
�B
�B
�B
�B
B
B
�B
�B
�B
�B
!�B
#�B
$B
%FB
%B
%B
%�B
&LB
*0B
,B
,�B
-CB
-�B
0UB
1�B
1�B
2aB
2aB
2�B
33B
3hB
5tB
6B
6B
7�B
9�B
:*B
;dB
;�B
<B
<6B
<B
<B
<B
<�B
>�B
>�B
?�B
@�B
A�B
A�B
B[B
B�B
B�B
C�B
C�B
C�B
C�B
C�B
C�B
C�B
CaB
DgB
E�B
G�B
HKB
HB
IB
IB
I�B
I�B
J#B
JXB
I�B
J�B
K^B
K�B
L0B
L�B
L�B
MB
L�B
L�B
MB
L�B
MB
MB
N�B
NB
NB
NpB
N<B
N�B
OBB
PHB
P�B
Q�B
R B
RTB
RTB
R�B
R�B
S&B
T,B
T�B
UgB
UgB
UgB
U�B
UgB
U�B
U�B
UgB
V9B
VmB
V�B
W�B
XB
W�B
XB
XEB
XEB
X�B
X�B
X�B
YKB
YKB
YKB
[�B
[WB
[WB
[WB
[WB
[WB
[#B
Z�B
[#B
[�B
]dB
]�B
^5B
^jB
^5B
^�B
_B
^�B
^�B
_B
_�B
_;B
_�B
`B
a|B
`�B
a�B
a�B
aHB
a�B
bNB
bNB
c�B
d�B
d�B
e`B
e�B
e�B
f2B
f�B
f�B
gB
gB
gmB
g�B
g�B
g�B
g�B
g�B
g�B
gmB
h
B
iB
h�B
h�B
jKB
j�B
jB
jB
k�B
m)B
l�B
l�B
l�B
m�B
m]B
m]B
m�B
m�B
m�B
n/B
n/B
n/B
n/B
n�B
p;B
p;B
qvB
qvB
q�B
rB
r|B
sB
sB
r�B
sB
tB
u%B
t�B
t�B
u�B
u�B
u�B
uZB
u�B
u�B
uZB
v+B
v`B
v+B
v+B
v+B
wfB
w2B
v�B
w2B
v�B
xB
y>B
yrB
y�B
y�B
y�B
zB
zB
z�B
z�B
z�B
z�B
{B
{B
{B
{B
{�B
{�B
{�B
{�B
|�B
|�B
|�B
|�B
}"B
}"B
}"B
}�B
}�B
}�B
~]B
~(B
~(B
~]B
~]B
~�B
~�B
~�B
~�B
~�B
.B
~�B
.B
�B
�B
�B
�B
�4B
�4B
�iB
�iB
�iB
�iB
�iB
��B
�B
�;B
�oB
��B
��B
��B
�B
�uB
��B
�uB
�GB
�{B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
�B
�SB
�%B
�YB
��B
�_B
��B
��B
��B
��B
��B
�fB
�1B
��B
�7B
�lB
��B
�B
��B
��B
�B
��B
��B
�B
�~B
�~B
�~B
�B
��B
�B
��B
�PB
��B
��B
��B
��B
�bB
� B
� B
�4B
�hB
�4B
��B
�:B
��B
��B
�B
�@B
�B
�uB
�FB
�MB
�B
�B
��B
��B
��B
��B
�B
�SB
��B
��B
�SB
��B
��B
�_B
��B
��B
��B
�1B
�eB
�eB
��B
�B
�B
�kB
�kB
�7B
��B
��B
�B
��B
��B
��B
��B
�CB
�CB
�CB
�xB
�xB
��B
��B
��B
�~B
��B
��B
��B
�!B
�!B
��B
�VB
�VB
�VB
��B
��B
��B
��B
��B
��B
�\B
��B
��B
�-B
��B
�bB
��B
��B
�4B
�4B
��B
��B
�4B
�:B
��B
�nB
�nB
�:B
��B
�B
�B
�LB
�LB
��B
��B
�LB
�LB
�RB
��B
��B
��B
�$B
�XB
�XB
�$B
��B
��B
��B
��B
�0B
�eB
��B
��B
��B
��B
��B
��B
�B
�6B
�6B
�6B
�kB
�kB
�kB
�B
��B
��B
�B
�B
��B
��B
��B
��B
�wB
��B
��B
��B
�wB
��B
�}B
�OB
��B
��B
�OB
�'B
�[B
�'B
�[B
�[B
�[B
��B
��B
��B
��B
�[B
��B
��B
��B
�hB
�hB
��B
��B
��B
�B
�nB
��B
��B
�B
�tB
��B
��B
��B
��B
�B
�B
�FB
�zB
��B
�B
�LB
��B
��B
�B
��B
��B
��B
��B
��B
��B
�$B
��B
�$B
�$B
�$B
�XB
�XB
��B
�XB
��B
�*B
�^B
�^B
��B
��B
��B
��B
��B
�jB
��B
��B
��B
��B
�B
�<B
�<B
��B
��B
��B
��B
��B
��B
�B
�BB
�B
��B
�B
��B
��B
�B
�B
�B
�B
�B
��B
�B
�HB
��B
�}B
��B
��B
�B
��B
��B
� B
� B
� B
��B
��B
��B
��B
��B
��B
�'B
�[B
��B
��B
��B
��B
�aB
ÖB
��B
��B
��B
�3B
ĜB
ĜB
ĜB
ĜB
��B
�B
�9B
�mB
ŢB
ŢB
�B
�?B
�?B
�tB
�tB
�tB
ƨB
��B
��B
��B
�B
�B
�EB
�zB
�EB
�zB
��B
��B
�B
�B
ȀB
ȀB
ȴB
ȴB
��B
��B
��B
��B
��B
�B
�B
R B
RTB
J�B
P�B
P}B
R�B
PB
RTB
QNB
O�B
P�B
RTB
P�B
O�B
Q�B
QNB
M�B
PB
QB
O�B
P}B
QNB
Q�B
O�B
PHB
Q�B
O�B
O�B
Q�B
P�B
N�B
QNB
PHB
O�B
Q�B
PB
OB
PHB
P�B
OvB
OB
QNB
O�B
OB
P�B
P}B
OBB
QNB
PHB
OB
QB
P�B
OvB
O�B
PB
PB
OB
O�B
P�B
P�B
OB
OB
Q�B
O�B
OvB
P}B
Q�B
O�B
OBB
Q�B
N�B
PB
P�B
QNB
PB
P�B
Q�B
RTB
N�B
QB
Q�B
P�B
O�B
P�B
R B
P�B
PB
Q�B
QNB
PHB
OvB
P�B
Q�B
O�B
PHB
P�B
QNB
O�B
PHB
Q�B
P�B
PHB
O�B
Q�B
PHB
N�B
Q�B
Q�B
OvB
QNB
QNB
PB
OB
QB
Q�B
O�B
PB
QNB
P�B
O�B
O�B
Q�B
Q�B
OBB
PB
P�B
Q�B
PHB
PHB
R B
P�B
O�B
O�B
P}B
O�B
Q�B
P�B
S�B
Q�B
P�B
R�B
S�B
R�B
S�B
P�B
T�B
PHB
O�B
P�B
RTB
PHB
P�B
R�B
R B
O�B
MB
U�B
TaB
S[B
QNB
T,B
TaB
Q�B
Q�B
S�B
R�B
R B
RTB
VmB
T�B
S�B
UgB
VB
RTB
S�B
WsB
UgB
T,B
T�B
S�B
T�B
T�B
[WB
VmB
UgB
T�B
QB
Q�B
R�B
R�B
Q�B
QB
NpB
XyB
X�B
X�B
X�B
X�B
ZQB
VmB
Y�B
Y�B
XyB
WsB
W�B
ZQB
ZQB
Y�B
Z�B
[WB
\]B
Y�B
X�B
ZQB
[#B
_B
^5B
\�B
h>B
bNB
a�B
jKB
e�B
a�B
aB
N�B
MB
TaB
LdB
S�B
J�B
EB
FB
IB
O�B
E�B
7�B
5B
F�B
4nB
7�B
;�B
2�B
1�B
0�B
2�B
5�B
8�B
;0B
:�B
@OB
B'B
CaB
C�B
DgB
F?B
I�B
MB
M6B
L�B
M�B
R�B
QB
P}B
N�B
P}B
PB
NB
N<B
NpB
P}B
P�B
V�B
e�B
g�B
o B
t�B
x8B
xlB
{B
|�B
{B
z�B
{B
|�B
}�B
}VB
|�B
�B
�iB
�B
��B
��B
�3B
�[B
��B
��B
�B
�9B
˒B
�&B
֡B
�2B
� B
�B
�B
�QB
�]B
�]B
�B
�GB
�	B�B�B�B@�BS�B_pBa�Be�BiDBs�Bt�By>B|�B~�B�B��B�_B��B��B��B��B��B��B��B�'B��B�FB��B��B��B��B��B�9B��B��B�B�TB33B�BxB�B�BxB �B"hB(�B/OB+B*0B0!B1�B0�B5�B5�B7LB5?B3hB5B7LB>BB?}B>�B8�B9$B<B=<B;0B;�B?B;0B=<B<jB?B?HBA BDgBK^BT�BJ�Bb�BU�BU�B[�B]�Bc�Bh�BjBj�BkBl�Bm�Bo�Bn/Bn�Bo5Bo Bp�BqvBtBu�Bw2B�SB�xB��B��B��B��B�B�OB�B��B�XB�B�<B�XB�jB��B�*B��B�0B�jB�jB��B�B�}B�B�<B�BB��B�mBǮB�B�pB��BǮBуB�0B��B�B��B�B�jB�dBуBΥBɺB̘B��B�B�TB�2B��B�B�tB��B�&BȀB��B�qB��B��B�XB��B�tB�B�tB�B��B�nB��B��B��B��B�B��B�LB�bB��B��B�'B��B�SB�B��Bx8B~�Bv�B|�Bt�Bt�Br|Bo5Bm]Bm]Bl"Bl�BiDBh�Bm]Bj�BbNBm�BdZB^B]dBW�BX�B\)Bd�BbBK)BE�BB�B?�BAUBM�B=BAUB9�B6zB5B<6BGEB;�B$@B(�B+�B 'BIB7�B �B)�B'�B�B	lBB��BB�B�TB  B��B��B��B�`B��B�BߤB� B�XB�EB��B��B�OBǮB�RB��B�zB�B�LB�wB�_B��B�B��B��B�B��B� B��B��B��B��B�rB�B{�B�Bz�Bu�B��B�IB{B_B[�B]�Bh�B\)Bs�BZ�BP�BM�BGBF�BF�BQ�B\�B8B1'B/�B0!B/�B.�B.�B.�B.IB-B,B+�B)�B*0B'�B*�B,qB&�B"�B&LB>�B5�B�B�B1BS&B
�B+BB
�.B 4BoB
�.B
��B
��B
��BYB  B
�B
�yB
��B
�B
�B
�B
�B
�B
��B
�MB
��B
�
B
�;B
�JB
��B
��B
�B
�'B
�qB
��B
�<B
�IB
��B
�B
��B
��B
�B
��B
�:B
�	B
��B
��B
�VB
x�B
�"B
|�B
o�B
lWB
q�B
m�B
k�B
jB
o5B
f�B
f�B
f�B
zDG�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�44444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444                                                                                                                                                                                                                                                                                                                             G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�44444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444                                                                                                                                                                                                                                                                                                                             G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�PRES            TEMP            PSAL            PRES            TEMP            PSAL                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                            SIO SOLO floats auto-correct mild pressure drift by zeroing the pressure sensor while on the surface. Additional correction was unnecessary in DMQC;                                                   PRES_ADJ_ERR: SBE sensor accuracy + resolution error     No significant temperature drift detected;                                                                                                                                                             TEMP_ADJ_ERR: SBE sensor accuracy + resolution error     Rapid salinity drift determined to be uncorrectable in DMQC;                                                                                                                                                                                                    SIO SOLO floats auto-correct mild pressure drift by zeroing the pressure sensor while on the surface. Additional correction was unnecessary in DMQC;                                                   PRES_ADJ_ERR: SBE sensor accuracy + resolution error     No significant temperature drift detected;                                                                                                                                                             TEMP_ADJ_ERR: SBE sensor accuracy + resolution error     Rapid salinity drift determined to be uncorrectable in DMQC;                                                                                                                                                                                                    202202042329482022020423294820220204232948202202042329482022020423294820220204232948SI  SI  ARFMARFM                                                                                                                                                2021061717245120210617172451IP  IP                                  G�O�G�O�G�O�G�O�G�O�G�O�                                AO  AO  ARGQARGQQCPLQCPL                                                                                                                                        2021062717010420210627170104QCP$QCP$                                G�O�G�O�G�O�G�O�G�O�G�O�5F03E           703E            AO  AO  ARGQARGQQCPLQCPL                                                                                                                                        2021062717010420210627170104QCF$QCF$                                G�O�G�O�G�O�G�O�G�O�G�O�8000            0               SI  SI  ARFMARFM                                                                                                                                                2022012609365020220126093650IP  IP                                  G�O�G�O�G�O�G�O�G�O�G�O�V3.1 profile    V3.1 profile    SI  SI  ARCAARCASIQCSIQCV2.1V2.1                                                                                                                                2022020423295220220204232952IP  IP                                  G�O�G�O�G�O�G�O�G�O�G�O�                                SI  SI  ARSQARSQOWC OWC V1.1V1.1CTD_for_DMQC_2021V01                                            CTD_for_DMQC_2021V01                                            2022020423295220220204232952IP  IP                                  G�O�G�O�G�O�G�O�G�O�G�O�                                SI  SI  ARDUARDU                                                                                                                                                2022020423295220220204232952IP  IP                                  G�O�G�O�G�O�G�O�G�O�G�O�                                